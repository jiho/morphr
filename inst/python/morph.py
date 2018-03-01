## I/O ----

# Read an image file and return a ndarray
def img_read(path):
    import skimage as im
    from skimage import io
    # read image
    img = io.imread(path, as_grey=True)
    # invert it and convert it to [0,1] (useful for future processing)
    img = 1 - im.img_as_float(img)
    return(img)

# Show an array as an image
def v(x):
    from skimage import io
    io.imshow(x)
    pass

# Write an ndarray as an image into a file
def img_write(path, x):
    import skimage as im
    from skimage import io
    # write it
    io.imsave(path, x)
    pass

## MEASURE ----

# Fast area computation from a RegionProperties object
def fast_area(x):
    import numpy as np
    return(np.sum(x._label_image[x._slice] == x.label))

# Extract measurements for the largest object in an image
def measure_largest(x, threshold=0):
    from skimage import measure
    import numpy as np
    # make binary
    xb = x > threshold
    xl = measure.label(xb, background=False, connectivity=2)
    # keep only the largest particle
    props = measure.regionprops(label_image=xl, intensity_image=x)
    areas = [fast_area(x) for x in props]
    props = props[np.argmax(areas)]
    return(props)

# Put a white pixel at the centroid (to visualise it)
def mark_centroid(x):
    # get centroid
    props = measure_largest(x)
    c = props.centroid
    # mark the corresponding pixel white
    c = [int(l) for l in c]
    x[c[0],c[1]] = 1
    return(x)

## TRANSFORM ----

# Measure the angle of an image and rotate it
def rotate(x):
    from skimage import transform
    import numpy as np
    # fit ellipse to largest object and rotate it horizontally
    props = measure_largest(x, threshold=0)
    angle = props.orientation * 180 / np.pi
    x = transform.rotate(x, angle=-angle, center=props.centroid, resize=True)
    # always put grey level centroid on the top left
    # = usually orient object with head on left and back on top
    props = measure_largest(x, threshold=0)
    if props.centroid[0] > x.shape[0]/2:
        x = np.flipud(x)
    if props.centroid[1] > x.shape[1]/2:
        x = np.fliplr(x)
    return(x)

# Compute padding along a dimension to center the centroid
# length    current size along this dimension
# centroid  current coordinarte of centroid in this dimenstion
# target    target dimension
def pad_to_center(length, centroid, target):
    # compute padding to put centroid in the center of the target image
    pad_before = int(round(target / 2 - centroid))
    # compute how much is left to pad on the right
    pad_after = int(target) - (pad_before + int(length))
    # error out if there is not enough space left
    if pad_after < 0:
        raise ValueError('target dimension too small')
    return((pad_before, pad_after))

# Center the centroid of array x within an array of shape (h,w)
def center(x, h, w):
    import numpy as np
    # measure the image and locate the centroid in height and width
    ih,iw = x.shape
    ch,cw = measure_largest(x, threshold=0).centroid
    # compute padding in height and width
    padh = pad_to_center(ih, ch, h)
    padw = pad_to_center(iw, cw, w)
    x = np.pad(x, (padh, padw), mode='constant')
    return(x)

# Extract the pixels corresponding to the largest object in an image
def autocrop(x, threshold=0):
    props = measure_largest(x, threshold)
    # extract its values = the crop
    xc = props.intensity_image
    return(xc)

## ACTUAL MORPHING ----

# Morph several images into one
def morph(paths, dest='', adjust_grey=False):
    import numpy as np
    import skimage as im
    from skimage import exposure as exp
    from scipy.optimize import minimize
    # read all files
    imgs = [img_read(f) for f in paths]
    #
    # 1. Rotate and superpose all images
    # rotate all images (and orient them)
    imgs = [rotate(i) for i in imgs]
    # compute max width and height to accomodate all images
    heights = [i.shape[0] for i in imgs]
    widths = [i.shape[1] for i in imgs]
    h = max(heights) * 2
    w = max(widths) * 2
    # center all images
    imgs = [center(i, h, w) for i in imgs]
    #
    # 2. Average and adjust grey level
    # compute the average image = the morph
    img = np.mean(imgs, axis=0)
    if adjust_grey:
        # compute the average of mean grey levels over all images
        # = this will be the target to match here
        mean_gray_one = np.mean([measure_largest(i).mean_intensity for i in imgs])
        # search for the best gamma correction
        def optim_contrast(g, args):
            img = args[0]; target = args[1]
            # constrast the image
            imgg = exp.adjust_gamma(img, gamma=g)
            # compare its mean grey to the target one
            mean_gray = measure_largest(imgg).mean_intensity
            criterion = abs(target - mean_gray)
            return(criterion)
        # search for the minimal difference
        res = minimize(optim_contrast, x0=1, args=((img, mean_gray_one),), method='L-BFGS-B', tol=0.05, bounds=((0.1, 1),))
        # compute the mean gray of the morph before correction
        # mean_grey_before = measure_largest(img).mean_intensity
        # adjust grey levels based on this optimized gamma
        img = exp.adjust_gamma(img, gamma=res.x[0])
        # compute mean gray after correction
        # mean_grey_after = measure_largest(img).mean_intensity
        # print(' target:', mean_gray_one, 'orig:', mean_grey_before, ' adj:', mean_grey_after, ' (gamma:', res.x[0], ')')
    #
    # 3. Finalise image
    # crop it
    img = autocrop(img)
    # invert the image and convert it to integer
    img = im.img_as_ubyte(1 - img)
    # write it if needed
    if dest != '':
        img_write(dest, img)
    # return it
    return(img)

# Morph a random selection of images from a directory into one
def morph_dir(path, n=15, write=True, adjust_grey=False):
    import os
    import numpy as np
    # list jpg files under path
    # files = [f for f in os.listdir(path) if f.endswith('.jpg')]
    files = os.listdir(path)
    # pick a few at random
    files = np.random.choice(files, min(n, len(files)), replace=False)
    # prepare the destination file
    if write:
        dest = path +'.jpg'
    else:
        dest = ''
    # compute the morphing (and write it as a file if needed)
    img = morph([os.path.join(path, f) for f in files], dest=dest, adjust_grey=adjust_grey)
    # return the morph
    return(img)

# ## TEST ----
#
# import os
# source = 'zooscanPtBWP2/level1/'
# # foo = morph_dir(os.path.join(source, 'Creseidae'), n=15, write=True)
# for taxon in [d for d in os.listdir(source) if os.path.isdir(os.path.join(source, d))]:
#     print(taxon)
#     foo = morph_dir(os.path.join(source, taxon), n=15, write=True)
