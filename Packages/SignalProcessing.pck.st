'From Cuis 5.0 of 7 November 2016 [latest update: #3190] on 19 October 2017 at 4:14:14 pm'!
'Description Image Processing and Signal Processing algorithms. FloatImage, Interpolation, Histograms, etc.'!
!provides: 'SignalProcessing' 1 86!
!requires: 'Goodies' 1 0 nil!
!requires: 'LinearAlgebra' 1 nil nil!
!requires: 'Statistics' 1 nil nil!
!requires: 'Collections-CompactArrays' 1 2 nil!
!requires: 'Graphics-Files-Additional' 1 nil nil!
!requires: 'ProbabilityDistributions' 1 nil nil!
!requires: 'Sound' 1 nil nil!
!classDefinition: #FloatImage category: #'SignalProcessing-Images'!
FloatMatrix subclass: #FloatImage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-Images'!
!classDefinition: 'FloatImage class' category: #'SignalProcessing-Images'!
FloatImage class
	instanceVariableNames: ''!

!classDefinition: #IrregularSampledImage category: #'SignalProcessing-Images'!
FloatImage subclass: #IrregularSampledImage
	instanceVariableNames: 'closestKnownX closestKnownY possibleTriPoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-Images'!
!classDefinition: 'IrregularSampledImage class' category: #'SignalProcessing-Images'!
IrregularSampledImage class
	instanceVariableNames: ''!

!classDefinition: #PerspectiveTransformation category: #'SignalProcessing-Images'!
GeometryTransformation variableWordSubclass: #PerspectiveTransformation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-Images'!
!classDefinition: 'PerspectiveTransformation class' category: #'SignalProcessing-Images'!
PerspectiveTransformation class
	instanceVariableNames: ''!

!classDefinition: #FloatImageTest category: #'SignalProcessing-Images'!
TestCase subclass: #FloatImageTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-Images'!
!classDefinition: 'FloatImageTest class' category: #'SignalProcessing-Images'!
FloatImageTest class
	instanceVariableNames: ''!

!classDefinition: #WaveletCodec category: #'SignalProcessing-SoundSynthesis'!
SoundCodec subclass: #WaveletCodec
	instanceVariableNames: 'fwt samplesPerFrame nLevels alpha beta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-SoundSynthesis'!
!classDefinition: 'WaveletCodec class' category: #'SignalProcessing-SoundSynthesis'!
WaveletCodec class
	instanceVariableNames: ''!

!classDefinition: #AccumulatedImageHistogram category: #'SignalProcessing-Images'!
Object subclass: #AccumulatedImageHistogram
	instanceVariableNames: 'tallies'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-Images'!
!classDefinition: 'AccumulatedImageHistogram class' category: #'SignalProcessing-Images'!
AccumulatedImageHistogram class
	instanceVariableNames: ''!

!classDefinition: #GeneralizedInterpolatingImage category: #'SignalProcessing-Images'!
Object subclass: #GeneralizedInterpolatingImage
	instanceVariableNames: 'width height coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-Images'!
!classDefinition: 'GeneralizedInterpolatingImage class' category: #'SignalProcessing-Images'!
GeneralizedInterpolatingImage class
	instanceVariableNames: ''!

!classDefinition: #BSplineInterpolatingImage category: #'SignalProcessing-Images'!
GeneralizedInterpolatingImage subclass: #BSplineInterpolatingImage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-Images'!
!classDefinition: 'BSplineInterpolatingImage class' category: #'SignalProcessing-Images'!
BSplineInterpolatingImage class
	instanceVariableNames: ''!

!classDefinition: #ShiftedLinearInterpolatingImage category: #'SignalProcessing-Images'!
GeneralizedInterpolatingImage subclass: #ShiftedLinearInterpolatingImage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-Images'!
!classDefinition: 'ShiftedLinearInterpolatingImage class' category: #'SignalProcessing-Images'!
ShiftedLinearInterpolatingImage class
	instanceVariableNames: ''!

!classDefinition: #ImageHistogram category: #'SignalProcessing-Images'!
Object subclass: #ImageHistogram
	instanceVariableNames: 'tallies'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-Images'!
!classDefinition: 'ImageHistogram class' category: #'SignalProcessing-Images'!
ImageHistogram class
	instanceVariableNames: ''!

!classDefinition: #ImageHistogram2 category: #'SignalProcessing-Images'!
Object subclass: #ImageHistogram2
	instanceVariableNames: 'tallies'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-Images'!
!classDefinition: 'ImageHistogram2 class' category: #'SignalProcessing-Images'!
ImageHistogram2 class
	instanceVariableNames: ''!

!classDefinition: #ImageHistogram3 category: #'SignalProcessing-Images'!
Object subclass: #ImageHistogram3
	instanceVariableNames: 'tallies'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-Images'!
!classDefinition: 'ImageHistogram3 class' category: #'SignalProcessing-Images'!
ImageHistogram3 class
	instanceVariableNames: ''!

!classDefinition: #MultiBandImage category: #'SignalProcessing-Images'!
Object subclass: #MultiBandImage
	instanceVariableNames: 'bands'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-Images'!
!classDefinition: 'MultiBandImage class' category: #'SignalProcessing-Images'!
MultiBandImage class
	instanceVariableNames: ''!

!classDefinition: #FFT category: #SignalProcessing!
Object subclass: #FFT
	instanceVariableNames: 'nu n sinTable permTable realData imagData window'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing'!
!classDefinition: 'FFT class' category: #SignalProcessing!
FFT class
	instanceVariableNames: ''!

!classDefinition: #FFT2D category: #SignalProcessing!
Object subclass: #FFT2D
	instanceVariableNames: 'realMatrix imagMatrix'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing'!
!classDefinition: 'FFT2D class' category: #SignalProcessing!
FFT2D class
	instanceVariableNames: ''!

!classDefinition: #FWT category: #SignalProcessing!
Object subclass: #FWT
	instanceVariableNames: 'alpha beta coeffs h g hTilde gTilde samples nSamples nLevels transform'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing'!
!classDefinition: 'FWT class' category: #SignalProcessing!
FWT class
	instanceVariableNames: ''!


!FloatImage commentStamp: '<historical>' prior: 0!
My instances are 1-band images suitable for Image Processing algorithms. Pixel values are 32-bit Float.

A glimpse of the things you can do:

	| tiff jpg component iTiff iJpg diff |
	tiff _ '../../4.2.04.tiff' asFileEntry form.
	jpg _ '../../4.2.04.jpg' asFileEntry form.
	jpg _ '../../4.2.04-95.jpg' asFileEntry form.
	component _ #green.
	iTiff _ FloatImage from32BitColorForm: tiff component: component.
	iJpg _ FloatImage from32BitColorForm: jpg component: component.
	diff _ iTiff - iJpg * 14 + 0.5.
	diff histogram plot.
!

!IrregularSampledImage commentStamp: '<historical>' prior: 0!
Some real, known, pixels are added one by one (pixel position is usually random). At any point, all the other pixels can quickly be interpolated.
Allows for progressive reconstruction, with higher quality, as new data is available.
Can compute Digital Voronoi diagrams and Digital Delaunay Triangulation.!

!PerspectiveTransformation commentStamp: '<historical>' prior: 0!
A perspective transformation for resampling images (for example). Can apply or correct for the perspective resulting from taking a photograph to a 2D object that is not perpendicular to the camera axis (for example, taking an image of the ground ahead of the camera).

http://docs.opencv.org/modules/imgproc/doc/geometric_transformations.html#warpperspective

!

!WaveletCodec commentStamp: '<historical>' prior: 0!
The Wavelet codec performs a wavelet transform on the original data.  It then achieves its compression by thresholding the transformed data, converting all values below a given magnitude to zero, and then run-coding the resulting data.  The run-coding provides automatic variable compression depending on the parameters chosen.

As is, this codec achieves reasonable reproduction at 10:1 compression, although the quality from the GSMCodec is definitely better.  I feel that the quality would be comparable if uLaw scaling were introduced prior to thresholding.

The nice thing about using wavelets is there are numerous factors to play with for better performance:
	nLevels - the "order" of the transform performed
	alpha and beta - these specify the wavelet shape (some are better for speech)
	the actual threshold used
By simply changing these parameters, one can easily vary the compression achieved from 5:1 to 50:1, and listen to the quality at each step.

The specific format for an encoded buffer is as follows:
	4 bytes: frameCount.
	4 bytes: samplesPerFrame.
	4 bytes: nLevels.
	4 bytes: alpha asIEEE32BitWord.
	4 bytes: beta asIEEE32BitWord.
	frameCount occurrences of...
		2 bytes: frameSize in bytes, not including these 2
			may be = 0 for complete silence, meaning no scale even.
		4 bytes: scale asIEEE32BitWord.
		A series of 1- or 2-byte values encoded as follows:
			0-111: 	a run of N+1 consecutive 0's;
			112-127:	a run of (N-112)*256 + nextByte + 1 consecutive 0's;
			128-255:	a 15-bit signed value = (N*256 + nextByte) - 32768 - 16384.!

!AccumulatedImageHistogram commentStamp: 'jmv 6/4/2015 10:17' prior: 0!
Used by Histogram Equalization.

======================================================
ImageHistogram, ImageHistogram2, ImageHistogram3, AccumulatedImageHistogram
are now obsolete. They have been superseded by Histogram and UnitaryMonotonicMap
======================================================!

!GeneralizedInterpolatingImage commentStamp: '<historical>' prior: 0!
A kind of FloatImage that can interpolate values at any Float position (inside its bounds). Allows for "Generalized Interpolation" as described in:
	P. Thevenaz, T. Blu, M. Unser, "Interpolation Revisited,"
	IEEE Transactions on Medical Imaging,
	vol. 19, no. 7, pp. 739-758, July 2000.
Available at: http://bigwww.epfl.ch/thevenaz/interpolation/

The basic idea is to use non-interpolating functions, meaning that instead of image samples, we store some pre-processed coefficients.
Subclasses implement specific pre-processing and interpolation.!

!BSplineInterpolatingImage commentStamp: '<historical>' prior: 0!
Cubic B-Spline Image Interpolation. Based on:
	P. Thevenaz, T. Blu, M. Unser,
	"Interpolation Revisited,"
	IEEE Transactions on Medical Imaging,
	vol. 19, no. 7, pp. 739-758, July 2000.
C code and paper are available at: http://bigwww.epfl.ch/thevenaz/interpolation/

I implement only the cubic case (degree=3), the most useful in practice.
The original C code does mirroring at the bounds. This version extends the edge values.

The original image was to be evaluated at integer indexes in (1 to: width) x (1 to height). My instance can be evaluated at any Float coordinates in (0.5 to: width+0.5) x (0.5 to: height+0.5). This is good for image zooming with proper bounds. See AffineTransformation>>#forImageZoom:!

!ShiftedLinearInterpolatingImage commentStamp: 'jmv 4/16/2013 13:40' prior: 0!
Shifted Linear Image Interpolation. Based on:
 	T. Blu, P. Thevenaz, M. Unser,
	"Linear Interpolation Revitalized,"
	IEEE Transactions on Image Processing,
	vol. 13, no. 5, pp. 710-719, May 2004.
C code and paper are available at: http://bigwww.epfl.ch/thevenaz/shiftedlin/

The original image was to be evaluated at integer indexes in (1 to: width) x (1 to height). My instance can be evaluated at any Float coordinates in (0.5 to: width+0.5) x (0.5 to: height+0.5). This is good for image zooming with proper bounds.

Warning - Notes
--------------------
1) It looks like there is a slight displacement of rebuilt images. Because of this, I (jmv) chose to use basisShift/2 displacement on interpolation, instead of the original basisShift. See #showApparentDisplacement.

2) Generally, results are very good. The paper says SNR is good up to 3/4 Nyquist frequency. But in practice, images look very sharp, especially very fine textures. It appears that the lost high frequency content is replaced by good looking aliasing.

3) Successive interpolations are not really stable. The image value at the neighbourhood of some points seems to diverge to positive infinity, and for others, to negative infinity. This effect is even visible at the great Java demo at http://bigwww.epfl.ch/demo/jshiftlinear/start.php , if one choses 7 iterations. See FloatImage>>#testRotations .!

!ImageHistogram commentStamp: 'jmv 6/4/2015 10:16' prior: 0!
An ImageHistogram records the number of pixels in an image for each pixel value (actually for a number of pixel value intervals).

See ImageHistogram2 for a possibly better approach.

Pixel values are assumed to lie in the [0.0 .. 1.0] interval. Being N the size of the tallies array, there are N slots. N-2 slots have extent 1/(N-1). The two slots at the edges are [0.0 .. 1/(2N-2)) and [1 - (1/(2N-2)) .. 1.0].

For example, lets assume an ImageHistogram with 256 slots. If we store the values for a FloatImage built from a 8-bit gray bmp file, then the slot intervals would be:
	[0.0/255 .. 0.5/255)		"Counts pixels with original value 0, interval extent 0.5"
	[0.5/255 .. 1.5/255)		"Counts pixels with original value 1, interval extent 1"
	[1.5/255 .. 2.5/255)		"Counts pixels with original value 2, interval extent 1"
	[2.5/255 .. 3.5/255)		"Counts pixels with original value 3, interval extent 1"
	[3.5/255 .. 4.5/255)		"Counts pixels with original value 4, interval extent 1"
	...
	[252.5/255 .. 253.5/255)	"Counts pixels with original value 253, interval extent 1"
	[253.5/255 .. 254.5/255)	"Counts pixels with original value 254, interval extent 1"
	[254.5/255 .. 255/255]		"Counts pixels with original value 255, interval extent 0.5"

These intervals, excluding the first and last, can be computed by:
| n |
n _ 256.
(2 to: n-1) collect: [ :i | (i-1/(n-1.0)-(1/(2*(n-1)))) to: (i-1/(n-1.0)+(1/(2*(n-1)))) count: 2 ].

In reality, the first and last slots will count also pixels with values less than 0.0 and greater than 1.0 respectively. I.e. they act as if the image was clipped. In this example, we can think that all slots have an interval of size 1, and that the first interval is [-0.5/255 .. 0.5/255) and that the last interval is [254.5/255 .. 255.5/255).

Each slot can count up to (2 raisedTo:32)-1 pixels. If it is ever needed to use larger counts, replace the WordArray by an Array initialized from zeros.

======================================================
ImageHistogram, ImageHistogram2, ImageHistogram3, AccumulatedImageHistogram
are now obsolete. They have been superseded by Histogram and UnitaryMonotonicMap
======================================================!

!ImageHistogram2 commentStamp: 'jmv 6/4/2015 10:16' prior: 0!
An ImageHistogram2 is a kind of image histogram, but it doesn't record exact pixel counts. They are especially useful for images with Float values, or when using significantly less histogram bins than pixel values.

Pixel values are assumed to lie in the [0.0 .. 1.0] interval. Being N the size of the tallies array, there are N slots. centered at values 0.0, 1/(N-1), 2/(N-1), ..., 1.0. A pixel with value exactly k/(N-1) will add 1.0 to the correspoinding tally. A pixel with value z such that k / (N-1) < z < (k+1) / (N-1) will contribute to 2 tallies, in inverse proportion to the distance to each of them.

In reality, the first and last slots will count also pixels with values less than 0.0 and greater than 1.0 respectively. I.e. they act as if the image was clipped.

This class had problems at the first and last bins when used for histogram equalization. The solution is ImageHistogran3 + AccumulatedImageHistogram

======================================================
ImageHistogram, ImageHistogram2, ImageHistogram3, AccumulatedImageHistogram
are now obsolete. They have been superseded by Histogram and UnitaryMonotonicMap
======================================================!

!ImageHistogram3 commentStamp: 'jmv 6/4/2015 10:16' prior: 0!
An ImageHistogram3 is a kind of image histogram, but it doesn't record exact pixel counts. They are especially useful for images with Float values, or when using significantly less histogram bins than pixel values.

Pixel values are assumed to lie in the [0.0 .. 1.0] interval. Being N the size of the tallies array, there are N slots or bins of equal width that span that interval. For example, for 5 bins:
[0.0 .. 0.2]
[0.2 .. 0.4]
[0.4 .. 0.6]
[0.6 .. 0.8]
[0.8 .. 1.0]

But more important is their center value. Bin i is centered at i-0.5 / N.
For example, for 5 bins:
(1 to: 5) collect: [ :i | i-0.5/5 ] 
#(0.1 0.3 0.5 0.7 0.9)

A pixel with value exactly k-0.5/N will add 1.0 to the correspoinding tally. Pixels to the in the first half of the first bin belong in the first bin. Same for pixels in the second half of the last bin. Any other pixel value will contribute to 2 tallies, in inverse proportion to the distance to their center.

======================================================
ImageHistogram, ImageHistogram2, ImageHistogram3, AccumulatedImageHistogram
are now obsolete. They have been superseded by Histogram and UnitaryMonotonicMap
======================================================!

!MultiBandImage commentStamp: 'jmv 7/23/2012 09:01' prior: 0!
a Multi Band Image is composed of several Single Band Images.

Each band can be used for a different spectral band (as in RGB color images, and multispectral and hyperspectral images). But a band can also mean panchromatic (black and white), opacity (translucent images), derived data such as image classification, or even extra data attached to each pixel by some user or application.

Each band can have a name (a Symbol) used to access it. It can also include a description, and maybe some arbitrary object for app-defined data.!

!FFT commentStamp: '<historical>' prior: 0!
This class implements the Fast Fourier Transform roughly as described on page 367
of "Theory and Application of Digital Signal Processing" by Rabiner and Gold.
Each instance caches tables used for transforming a given size (n = 2^nu samples) of data.

It would have been cleaner using complex numbers, but often the data is all real.!

!FFT2D commentStamp: '<historical>' prior: 0!
My instances provide for 2D FFT on FloatMatrix'es and FloatImages. I rely on FFT for succesive transform by rows and by columns.!

!FWT commentStamp: '<historical>' prior: 0!
This class implements the Fast Wavelet Transform.  It follows Mac Cody's article in Dr. Dobb's Journal, April 1992.  See also... 
	http://www.dfw.net/~mcody/fwt/fwt.html

Notable features of his implementation include...
1.  The ability to generate a large family of wavelets (including the Haar (alpha=beta) and Daubechies) from two parameters, alpha and beta, which range between -pi and pi.
2.  All data arrays have 5 elements added on to allow for convolution overrun with filters up to 6 in length (the max for this implementation).
3.  After a forward transform, the detail coefficients of the deomposition are found in transform at: 2*i, for i = 1, 2, ... nLevels;  and the approximation coefficients are in transform at: (2*nLevels-1).  these together comprise the complete wavelet transform.

The following changes from cody's listings should also be noted...
1.  The three DotProduct routines have been merged into one.
2.  The four routines WaveletDecomposition, DecomposeBranches, WaveletReconstruction, ReconstructBranches have all been merged into transformForward:.
3.  All indexing follows the Smalltalk 1-to-N convention, naturally.!

!Matrix methodsFor: '*SignalProcessing-resampling' stamp: 'jmv 5/4/2016 10:13'!
bilinearInterpolatedValueAtX: x y: y
	"Evaluate a bilinear (i.e. cheap) interpolation
	Answer is always a Float, regardless of Matrix tyle
	Like OpenCV's CV_INTER_LINEAR
	Answer 0.0 if outside image bounds: [1 .. width] x [1 .. height]"

	| w interpolated xWeight1 xWeight0 yWeight1 yWeight0 xIndex0 xIndex1 yIndex0 yIndex1 |
	xIndex0 _ x truncated. 		"Could be #floor. But as we only care for values >=1, it is the same. But faster."
	xIndex0 < 1 ifTrue: [ ^0.0 ].
	xIndex0 > width ifTrue: [ ^0.0 ].
	(xIndex0 = width and: [ x > width ]) ifTrue: [ ^0.0 ].
	xIndex1 _ xIndex0 = width 	"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ xIndex0 + 1 ]
		ifTrue: [ xIndex0 ].

	yIndex0 _ y truncated.			"Could be #floor. But as we only care for values >=1, it is the same. But faster."
	yIndex0 < 1 ifTrue: [ ^0.0 ].
	yIndex0 > height ifTrue: [ ^0.0 ].
	(yIndex0 = height and: [ y > height ]) ifTrue: [ ^0.0 ].
	yIndex1 _ yIndex0 = height 	"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ yIndex0 + 1 ]
		ifTrue: [ yIndex0 ].

	xWeight1 _ x - xIndex0.
	xWeight0 _ 1.0 - xWeight1.

	yWeight1 _ y - yIndex0.
	yWeight0 _ 1.0 - yWeight1.

	"/* perform interpolation */"
	w _ (xWeight0 * (self x: xIndex0 y: yIndex0)) +
		(xWeight1 * (self x: xIndex1 y: yIndex0)).
	interpolated _ yWeight0 * w.

	w _ (xWeight0 * (self x: xIndex0 y: yIndex1)) +
		(xWeight1 * (self x: xIndex1 y: yIndex1)).
	interpolated _ yWeight1 * w + interpolated.

	^interpolated! !

!FloatMatrix methodsFor: '*SignalProcessing' stamp: 'jmv 4/24/2015 11:06'!
convolutionLinearWith: aForm
	"Apply a linear convolution with self to aForm. Warning: Slow.
	It is usually preferred to do a circular convolution via FFT and take care of border effects."
	"
	| f m |
	f _ Form fromUser.
	m _ FloatMatrix m: 7 n: 7.
	m fillWithGaussianKernelSigmaY: 1 sigmaX: 1.
	m _ m moveOriginToCenter: true.
	(m convolutionLinearWith: f) display
	"

	| resultWidth resultHeight result xx yy blue c green red k deltaI deltaJ |
	resultWidth _ aForm width.
	resultHeight _ aForm height.
	k _ 1.0 / elements sum.
	result _ Form extent: resultWidth@resultHeight depth: 32.
	deltaI _ height // 2 + 1.
	deltaJ _ width // 2 + 1.
	1 to: resultHeight do: [ :y |
		1 to: resultWidth do: [ :x |
			red _ 0.0.
			green _ 0.0.
			blue _ 0.0.
			1 to: height do: [ :j |
				1 to: width do: [ :i |
					xx _ x-i+deltaI.
					yy _ y-j+deltaJ.
					(xx > 0) & (xx <= aForm width) & (yy > 0) & (yy <= aForm height)
						ifTrue: [
							c _ aForm colorAt: xx@yy.
							red _ c red * (self i: i j: j) + red.
							green _ c green * (self i: i j: j) + green.
							blue _ c blue * (self i: i j: j) + blue ]]].
			red _ red * k min: 1.0 max: 0.0.
			green _ green * k min: 1.0 max: 0.0.
			blue _ blue * k min: 1.0 max: 0.0.
			result colorAt: x@y put:
				(Color r: red g: green b: blue).
		].
	].
	^result! !

!FloatMatrix methodsFor: '*SignalProcessing' stamp: 'jmv 8/28/2015 11:09'!
fftImaginary: aMatrix forward: forward
	"Compute complex 2d FFT, being me the real part and aMatrix the imaginary part of the data to transform.
	Frequency zero is at topLeft, i=1;j=1.
	"
	| transform |
	transform _ FFT2D new.
	transform realData: self imagData: aMatrix.
	transform transformForward: forward! !

!FloatMatrix methodsFor: '*SignalProcessing' stamp: 'jmv 9/18/2007 13:56'!
fftRealForward: forward
	"Answer real part of 2d FFT, being me the real part of the data to transform.
	I.e. no imaginary part on input, and imaginary part on output is discarded"
	| transform |
	transform _ FFT2D new.
	transform realData: self.
	transform transformForward: forward! !

!FloatMatrix methodsFor: '*SignalProcessing' stamp: 'jmv 4/16/2015 12:27'!
fillWithGaussianKernelSigmaY: sigmaY sigmaX: sigmaX
	"
	Fill the receiver with a gaussian function. sigmaX and sigmaY are the standard deviation of the gaussian, measured in elements (i.e. 'pixels').
	Origin is at topLeft, appropriate for doing a convolution.
	#moveOriginToCenter to make it more confortable to see.

	| f |
	f _ FloatMatrix height: 16 width: 16.
	f fillWithGaussianKernelSigmaY: 1 sigmaX: 1.
	f _ f moveOriginToCenter: true.
	f asForm display.
	FFT new plot: (f i: f height//2+1) elements in: (0@0 extent: 200@100) color: Color red min: 0 max: 1

	| f |
	f _ FloatMatrix height: 1000 width: 1000.
	f fillWithGaussianKernelSigmaY: 22 sigmaX: 12.5.
	f _ f moveOriginToCenter: true.
	f asForm display.
	FFT new plot: (f i: f height//2+1) elements in: (0@0 extent: 200@100) color: Color red min: 0 max: 1
	"
	| supportH supportW v |
	"15 sigmas is enough for the exponential function to underflow in 32 bit Float
	(and gives a big performance gain compared with filling everything with zeros)"
	supportH _ height//2 min: (15 * sigmaY) ceiling.
	supportW _ width//2 min: (15 * sigmaX) ceiling.

	"0@0"
	self i: 1 j: 1 put: 1.

	"j@0"
	1 to: supportW do: [ :j |
		v _ self gaussianI: 0 j: j sigmaY: sigmaY sigmaX: sigmaX.
		self i: 1 j: j+1 put: v .
		self i: 1 j: width-j+1 put: v ].

	"i@0"
	1 to: supportH do: [ :i |
		v _ self gaussianI: i j: 0 sigmaY: sigmaY sigmaX: sigmaX.
		self i: i+1 j: 1 put: v.
		self i: height-i+1 j: 1 put: v ].

	"i@j"
	1 to: supportH do: [ :i |
		1 to: supportW do: [ :j |
			v _ self gaussianI: i j: j sigmaY: sigmaY sigmaX: sigmaX.
			self i: i+1 j: j+1 put: v.
			self i: i+1 j: width-j+1 put: v.
			self i: height-i+1 j: j+1 put: v.
			self i: height-i+1 j: width-j+1 put: v ]]! !

!FloatMatrix methodsFor: '*SignalProcessing' stamp: 'jmv 4/27/2015 15:34'!
gaussianI: i j: j sigmaY: sigmaY sigmaX: sigmaX
	" Comment based on http://en.wikipedia.org/wiki/Gaussian_function
	| ary expectedValue r standardDeviation a b c f x |
	expectedValue _ 0.0.
	standardDeviation _ 1.0.
	ary _ FloatArray new: 1000.
	1 to: 1000 do: [ :i |
		x _ i - 500 / 200.
		a _ 1 / (standardDeviation * (2.0 * Float pi )sqrt).
		b _ expectedValue.
		c _ standardDeviation.
		f _ ((x-b) squared / (2.0 * c squared)) negated exp * a.
		ary at: i put: f ].
	r _ 10@10 extent: 100@50.
	FFT new plot: ary in: r color: Color blue min: 0 max: ary max.
	"
	^ (((1.0*j/sigmaX) squared + (1.0*i/sigmaY) squared) / 2) negated exp! !

!FloatMatrix methodsFor: '*SignalProcessing' stamp: 'jmv 4/16/2015 12:25'!
lowPassFilter
	"Apply a low pass filtar to remove the high half frequency content.
	width and height must be an integer power of 2.
	Before using this, reevaluate assumtions, etc. This is not a good filter by most measures..."
	
	| imaginary transform blur |
	imaginary _ FloatMatrix height: height width: width.
	transform _ FFT2D new.
	transform realData: self imagData: imaginary.
	transform transformForward: true.

	blur _ FloatMatrix height: height width: width.
	blur fillWithGaussianKernelSigmaY: 16 sigmaX: 16.
	self elements *= blur elements.
	imaginary elements *= blur elements.

	transform transformForward: false! !

!FloatMatrix methodsFor: '*SignalProcessing' stamp: 'jmv 8/17/2011 13:21'!
normalizeMax
	"An image can be said to be properly exposed when the maximum value is 1.0"
	elements /= elements max! !

!FloatMatrix methodsFor: '*SignalProcessing' stamp: 'jmv 8/17/2011 13:15'!
normalizeSum
	"A filter is normalized when its elements add to 1.0"
	elements /= elements sum! !

!FloatMatrix methodsFor: '*SignalProcessing' stamp: 'jmv 8/26/2011 10:03'!
scatterPlotWith: aFloatMatrix size: n
	"Answer a FloatMatrix that represents a scatter plot of the receiver with the argument.
	Useful to study the correlation between the distributions of the values of the matrices."

	"A scatter plot is done by taking the bands (or color) by pairs, and doing a sort of
	bidimensional histogram. For each pair of pixel values (in each pair of colors) the total
	number of pixels is recorded."

	| plotSize scatterPlot max min scale v1 v2 |
	plotSize _ n.
	scatterPlot _ FloatMatrix m: plotSize n: plotSize.
	max _ self max max: aFloatMatrix max.
	min _ self min min: aFloatMatrix min.
	scale _ max = min
		ifFalse: [ plotSize / (max-min) asFloat ]
		ifTrue: [ 1 ].
	"This maping gives values between 0 and plotSize-1"
	1 to: height do: [ :i |
		1 to: width do: [ :j |
			v1 _ ((scale * ((self i: i j: j)-min)) truncated min: plotSize-1) max: 0.
			v2 _ ((scale * ((aFloatMatrix i: i j: j)-min)) truncated min: plotSize-1) max: 0.
			scatterPlot i: plotSize-v1 j: v2+1 put: (scatterPlot  i: plotSize-v1 j: v2+1) + 1 ]].

	^scatterPlot! !

!FloatMatrix methodsFor: '*SignalProcessing' stamp: 'jmv 4/24/2015 10:26'!
subsampled
	| answer |
	answer _ self appropriateResultClass height: height//2 width: width//2.
	1 to: answer height do: [ :i |
		1 to: answer width do: [ :j |
			answer i: i j: j put: (self i: i*2 j: j*2) ]].
	^ answer! !

!FloatMatrix methodsFor: '*SignalProcessing' stamp: 'jmv 4/24/2015 10:26'!
subsampled: n
	| answer |
	answer _ self appropriateResultClass height: height//n width: width//n.
	1 to: answer height do: [ :i |
		1 to: answer width do: [ :j |
			answer i: i j: j put: (self i: i*n j: j*n) ]].
	^ answer! !

!Form class methodsFor: '*SignalProcessing-examples' stamp: 'jmv 6/2/2016 20:13'!
lena
	"
	(Base64MimeConverter mimeEncode: ((FileStream readOnlyFileNamed: 'Lena.png') binary)) upToEnd
	Form lena display
	"
	^(Form fromBinaryStream:
		'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
Krf7W2XyH8kZofGsb0wZAAAAAElFTkSuQmCC'
				base64Decoded asByteArray readStream) asGrayForm! !

!Form class methodsFor: '*SignalProcessing-examples' stamp: 'jmv 6/2/2016 19:56'!
lenaColor64
	"
	('LenaColor64.png' asFileEntry readStreamDo: [ :strm | strm binary. Base64MimeConverter mimeEncode: strm ]) upToEnd
	Form lenaColor64 display
	"
	^Form fromBinaryStream:
		'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'
			base64Decoded asByteArray readStream! !

!JPEGReadWriter2 class methodsFor: '*SignalProcessing-examples' stamp: 'jmv 6/3/2016 17:38'!
printCompressionVersusQuality
	"
	JPEGReadWriter2 printCompressionVersusQuality
	"
	| bytes lena n rebuilt |
	lena _ GrayForm lena.
	n _ lena boundingBox width * lena boundingBox height.
	-1 to: 100 do: [ :i |
		bytes _ JPEGReadWriter2 new compress: lena quality: i progressiveJPEG: false.
		rebuilt _ JPEGReadWriter2 new uncompress: bytes into: nil.
		{i. bytes size. (lena pixelCompare: rebuilt boundingBox with: rebuilt at: 0@0) / n asFloat} print ].! !

!AffineTransformation class methodsFor: '*SignalProcessing-Images' stamp: 'jmv 1/27/2015 13:48'!
forImageRotation: radians zoom: scale originalExtent: originalExtent
	"
	Good for processing FloatImages

	Assume that a pixel covers a square centered at its coordinates, with area 1@1.
	Also assume that origin is 1@1 (this is true for FloatImage, but not for Form!!)
	
	Apply enough displacement to avoid clipping the result

	| original t e |
	original _ FloatImage checkerboard: 100@100 n: 25.
	t _ AffineTransformation forImageRotation: -0.2 zoom: 2.23 originalExtent: original extent.
	e _ original resultExtentFor: t.
	(original  areaTransformedBy: t resultExtent: e) display. (Delay forSeconds: 0.3) wait.
	(original  bSplineTransformedBy: t resultExtent: e) display. (Delay forSeconds: 0.3) wait.
	(original  bicubicTransformedBy: t resultExtent: e) display. (Delay forSeconds: 0.3) wait.
	(original  bilinearTransformedBy: t resultExtent: e) display. (Delay forSeconds: 0.3) wait.
	(original  lanczosTransformedBy: t resultExtent: e) display. (Delay forSeconds: 0.3) wait.
	(original  nearestNeighborTransformedBy: t resultExtent: e) display. (Delay forSeconds: 0.3) wait.
	(original  shiftedLinearTransformedBy: t resultExtent: e) display. (Delay forSeconds: 0.3) wait.
	"

	| halfOrig t1 r halfResult |
	t1 _ (AffineTransformation withRadians: radians) composedWith:
				(AffineTransformation withScale: scale).
	r _ t1 displayBoundsOfTransformOf: (0.5@0.5 extent: originalExtent).
	halfOrig _ originalExtent / 2.0+0.5.
	halfResult _ r extent / 2.0+0.5.
	^ (AffineTransformation withTranslation: halfResult) composedWith:
			(t1 composedWith:
					(AffineTransformation withTranslation: halfOrig negated))! !

!AffineTransformation class methodsFor: '*SignalProcessing-Images' stamp: 'jmv 1/27/2015 12:46'!
forImageZoom: scale
	"
	Good for doing zoom in and zoom out of images.
	scale is the scale factor to apply (i.e. 0.5 means result is half the size of original image).

	Assume that a pixel covers a square centered at its coordinates, with area 1@1.
	Also assume that origin is 1@1 (this is true for FloatImage, but not for Form!!)
	Therefore, map 0.5@0.5 to 0.5@0.5.
	(AffineTransformation forImageZoom: 5) transform: 0.5@0.5
	"
	| dummyExtent |
	dummyExtent _ 100@100.
	^self transformFrom: (0.5@0.5 extent: dummyExtent) to: (0.5@0.5 extent: scale*dummyExtent)! !

!LoopedSampledSound methodsFor: '*SignalProcessing' stamp: 'jm 8/18/1998 08:19'!
copyDownSampledLowPassFiltering: doFiltering
	"Answer a copy of the receiver at half its sampling rate. The result consumes half the memory space, but has only half the frequency range of the original. If doFiltering is true, the original sound buffers are low-pass filtered before down-sampling. This is slower, but prevents aliasing of any high-frequency components of the original signal. (While it may be possible to avoid low-pass filtering when down-sampling from 44.1 kHz to 22.05 kHz, it is probably essential when going to lower sampling rates.)"

	^ self copy downSampleLowPassFiltering: doFiltering
! !

!LoopedSampledSound methodsFor: '*SignalProcessing' stamp: 'jm 8/18/1998 08:11'!
downSampleLowPassFiltering: doFiltering
	"Cut my sampling rate in half. Use low-pass filtering (slower) if doFiltering is true."
	"Note: This operation loses information, and modifies the receiver in place."

	| stereo newLoopLength |
	stereo _ self isStereo.
	leftSamples _ leftSamples downSampledLowPassFiltering: doFiltering.
	stereo
		ifTrue: [rightSamples _ rightSamples downSampledLowPassFiltering: doFiltering]
		ifFalse: [rightSamples _ leftSamples].
	originalSamplingRate _ originalSamplingRate / 2.0.
	loopEnd odd
		ifTrue: [newLoopLength _ (self loopLength / 2.0) + 0.5]
		ifFalse: [newLoopLength _ self loopLength / 2.0].
	firstSample _ (firstSample + 1) // 2.
	lastSample _ (lastSample + 1) // 2.
	loopEnd _ (loopEnd + 1) // 2.
	scaledLoopLength _ (newLoopLength * LoopIndexScaleFactor) asInteger.
	scaledIndexIncr _ scaledIndexIncr // 2.
! !

!LoopedSampledSound methodsFor: '*SignalProcessing' stamp: 'jm 8/18/1998 07:49'!
fftAt: startIndex
	"Answer the Fast Fourier Transform (FFT) of my samples (only the left channel, if stereo) starting at the given index."

	| availableSamples fftWinSize |
	availableSamples _ (leftSamples size - startIndex) + 1.
	fftWinSize _ 2 raisedTo: (((availableSamples - 1) log: 2) truncated + 1).
	fftWinSize _ fftWinSize min: 4096.
	fftWinSize > availableSamples ifTrue: [fftWinSize _ fftWinSize / 2].
	^ self fftWindowSize: fftWinSize startingAt: startIndex
! !

!LoopedSampledSound methodsFor: '*SignalProcessing' stamp: 'jm 8/18/1998 07:48'!
fftWindowSize: windowSize startingAt: startIndex
	"Answer a Fast Fourier Transform (FFT) of the given number of samples starting at the given index (the left channel only, if stereo). The window size will be rounded up to the nearest power of two greater than the requested size. There must be enough samples past the given starting index to accomodate this window size."

	| nu n fft |
	nu _ ((windowSize - 1) log: 2) truncated + 1.
	n _ 2 raisedTo: nu.
	fft _ FFT new nu: nu.
	fft realData: ((startIndex to: startIndex + n - 1) collect: [:i | leftSamples at: i]).
	^ fft transformForward: true.
! !

!LoopedSampledSound methodsFor: '*SignalProcessing' stamp: 'jm 8/18/1998 09:26'!
highestSignificantFrequencyAt: startIndex
	"Answer the highest significant frequency in the sample window starting at the given index. The a frequency is considered significant if it's power is at least 1/50th that of the maximum frequency component in the frequency spectrum."

	| fft powerArray threshold indices |
	fft _ self fftAt: startIndex.
	powerArray _ self normalizedResultsFromFFT: fft.
	threshold _ powerArray max / 50.0.
	indices _ (1 to: powerArray size) select: [:i | (powerArray at: i) > threshold].
	^ originalSamplingRate / (fft samplesPerCycleForIndex: indices last)
! !

!LoopedSampledSound methodsFor: '*SignalProcessing' stamp: 'jm 8/16/1998 17:48'!
normalizedResultsFromFFT: fft
	"Answer an array whose size is half of the FFT window size containing power in each frequency band, normalized to the average power over the entire FFT. A value of 10.0 in this array thus means that the power at the corresponding frequences is ten times the average power across the entire FFT."

	| r avg |
	r _ (1 to: fft realData size // 2) collect:
		[:i | ((fft realData at: i) squared + (fft imagData at: i) squared) sqrt].
	avg _ r sum / r size.
	^ r collect: [:v | v / avg].
! !

!SampledSound methodsFor: '*SignalProcessing' stamp: 'di 9/6/2000 20:48'!
sonogramMorph: height from: start to: stop nPoints: nPoints
	"FYI:  It is very cool that we can do this, but for sound tracks on a movie,
	simple volume is easier to read, easier to scale, and way faster to compute.
	Code preserved here just in case it makes a useful example."
	"In an inspector of a samplesSound...
		self currentWorld addMorph: (self sonogramMorph: 32 from: 1 to: 50000 nPoints: 256)
	"
	| fft sonogramMorph data width |
	fft _ FFT new: nPoints.
	width _ stop-start//nPoints.
	sonogramMorph _ Sonogram new
			extent: width@height
			minVal: 0.0
			maxVal: 1.0
			scrollDelta: width.
	start to: stop-nPoints by: nPoints do:
		[:i |
		data _ fft transformDataFrom: samples startingAt: i.
		data _ data collect: [:v | v sqrt].  "square root compresses dynamic range"
		data /= 200.0.
		sonogramMorph plotColumn: data].
	^ sonogramMorph
	
! !

!FloatImage methodsFor: 'arithmetic-answer new' stamp: 'jmv 7/27/2012 18:16'!
* aFloatImageOrNumber
	"Answer a new FloatImage. Don't affect receiver or argument in any way."

	^self copy *= aFloatImageOrNumber! !

!FloatImage methodsFor: 'arithmetic-modify self' stamp: 'jmv 8/23/2014 23:37'!
*= aFloatImageOrNumber
	"Multiply receiver by an image (pixel by pixel) or number"

	aFloatImageOrNumber isNumber
		ifTrue: [ elements *= aFloatImageOrNumber ]
		ifFalse: [
			self validateSameSize: aFloatImageOrNumber.
			elements *= aFloatImageOrNumber elements ]! !

!FloatImage methodsFor: 'arithmetic-answer new' stamp: 'jmv 7/27/2012 18:16'!
+ aFloatImageOrNumber
	"Answer a new FloatImage. Don't affect receiver or argument in any way."

	^self copy += aFloatImageOrNumber! !

!FloatImage methodsFor: 'arithmetic-modify self' stamp: 'jmv 8/23/2014 23:38'!
+= aFloatImageOrNumber
	"Add an image (pixel by pixel) or number to receiver"

	aFloatImageOrNumber isNumber
		ifTrue: [ elements += aFloatImageOrNumber ]
		ifFalse: [
			self validateSameSize: aFloatImageOrNumber.
			elements += aFloatImageOrNumber elements ]! !

!FloatImage methodsFor: 'arithmetic-answer new' stamp: 'jmv 7/27/2012 18:16'!
- aFloatImageOrNumber
	"Answer a new FloatImage. Don't affect receiver or argument in any way."

	^self copy -= aFloatImageOrNumber! !

!FloatImage methodsFor: 'arithmetic-modify self' stamp: 'jmv 9/18/2017 17:31:33'!
-= aFloatImageOrNumber
	"Subtract an image (pixel by pixel) or number to receiver"

	aFloatImageOrNumber isNumber
		ifTrue: [ elements -= aFloatImageOrNumber ]
		ifFalse: [
			self validateSameSize: aFloatImageOrNumber.
			elements -= aFloatImageOrNumber elements ]! !

!FloatImage methodsFor: 'arithmetic-answer new' stamp: 'jmv 7/27/2012 18:16'!
/ aFloatImageOrNumber
	"Answer a new FloatImage. Don't affect receiver or argument in any way."

	^self copy /= aFloatImageOrNumber! !

!FloatImage methodsFor: 'arithmetic-modify self' stamp: 'jmv 8/23/2014 23:38'!
/= aFloatImageOrNumber
	"Divide receiver by an image (pixel by pixel) or number"

	aFloatImageOrNumber isNumber
		ifTrue: [ elements /= aFloatImageOrNumber ]
		ifFalse: [
			self validateSameSize: aFloatImageOrNumber.
			elements /= aFloatImageOrNumber elements ]! !

!FloatImage methodsFor: 'metrics' stamp: 'jmv 9/2/2014 13:22'!
acutance
	"Measure image acutance."
	| aux eAcutance nAcutance neAcutance nwAcutance |
	aux _ FloatImage extent: self extent.
	eAcutance _ self acutanceAt: 1@0 aux: aux.
	neAcutance _ (self acutanceAt: 1@1 aux: aux) * 0.75.
	nAcutance _ self acutanceAt: 0@1 aux: aux.
	nwAcutance _ (self acutanceAt: -1@1 aux: aux) * 0.75.
	^((eAcutance max: neAcutance) max: nAcutance) max: nwAcutance! !

!FloatImage methodsFor: 'metrics' stamp: 'jmv 3/25/2015 09:48'!
acutanceAt: delta aux: auxImage
	"Note: auxImage extent must equal self extent
		displacement is integer!!
	"

	auxImage fillWith: self cheaplyDisplaced: delta.
	auxImage -= self.
	auxImage *= auxImage.
	auxImage replaceValues: [ :i :j :v |
		((i between: delta y+1 and: height + delta y) and: [
			(j between: delta x+1 and: width + delta x) ])
				ifTrue: [v sqrt] 
				ifFalse: [0]].
	^auxImage mean! !

!FloatImage methodsFor: 'noise generation' stamp: 'jmv 7/19/2016 12:08:32'!
addNormalNoise: standardDeviation
	| random |
	random _ NormalProbabilityDistribution mean: 0.0 standardDeviation: standardDeviation.
	self replaceValues: [ :i :j :v | random nextRandomNumber + v ]! !

!FloatImage methodsFor: 'dynamic range operations' stamp: 'jmv 6/3/2015 21:49'!
applyMap: map
	self replaceValues: [ :i :j :v | map valueAt: (self i: i j: j)]! !

!FloatImage methodsFor: 'dynamic range operations' stamp: 'jmv 6/3/2015 22:20'!
applyMapInverse: map
	self replaceValues: [ :i :j :v | map inverseValueAt: (self i: i j: j)]! !

!FloatImage methodsFor: 'interpolation' stamp: 'jmv 3/12/2017 19:28:39'!
areaFilteredValueAtX: x y: y halfWidth: hw halfHeight: hh
	"Answer an interpolated value at (most likely) non-integer x@y.
	x and y can be any Float values in in [1.0 .. width] and [1.0 .. height].
	
	Akin to OpenCV's CV_INTER_AREA.
	Applies a step filter over the samples in the receiver that lie in a rectangle centered at x@y, as specified by hw and hh.
	
	Note: We could also consider the pixels in the receiver as squares and compute area intersection with result pixel.
	Results would be slightly better. But here we are focused on speed. If you need higher quality, please use Lanczos.

	Computationally very cheap.
	Avoids aliasing while reasonably keeps detail.
	Gives very good results for strong subsampling (zoom out, scale factors less than 0.4).

	Compute parameters as (scale usually <= 0.25):
		hw _ 1 / scale / 2.0.
		hh _ 1 / scale / 2.0.
	or (resultWidth usually much smaller than original, same for height):
		hw _ originalWidth / resultWidth / 2.0.
		hh _ originalHeight / resultHeight / 2.0.
	"
	| xStart xStop yStart yStop sum xIndex0 yIndex0 |

	"Validate inside some reasonable bounds. We allow extending up to 0.8 in any direction"
	xIndex0 _ x floor - 1.
	"x < 0.2 ifTrue: [ ^0.0 ]."			"Do it with integer arithmetic if at all possible"
	xIndex0 < -1 ifTrue: [ ^0.0 ].
	(xIndex0 = -1 and: [x < 0.2]) ifTrue: [ ^0.0 ].
	"x > (width + 0.8) ifTrue: [ ^0.0 ]."	"Do it with integer arithmetic if at all possible"
	xIndex0 > width ifTrue: [ ^0.0 ].
	(xIndex0 = width and: [x > (width + 0.8)]) ifTrue: [ ^0.0 ].
	yIndex0 _ y floor - 1.
	"y < 0.2 ifTrue: [ ^0.0 ]."			"Do it with integer arithmetic if at all possible"
	yIndex0 < -1 ifTrue: [ ^0.0 ].
	(yIndex0 = -1 and: [y < 0.2]) ifTrue: [ ^0.0 ].
	"y > (height + 0.8) ifTrue: [ ^0.0 ]."	"Do it with integer arithmetic if at all possible"
	yIndex0 > height ifTrue: [ ^0.0 ].
	(yIndex0 = height and: [y > (height + 0.8)]) ifTrue: [ ^0.0 ].

	xStart _ (x - hw) ceiling max: 1.
	xStop _ (x + hw) floor min: width.
	yStart _ (y - hh) ceiling max: 1.
	yStop _ (y + hh) floor min: height.

	"Apply filter."
	sum _ 0.0.
	yStart to: yStop do: [ :yyy |
		xStart to: xStop do: [ :xxx |
			sum _ (self x: xxx y: yyy) + sum ]].
	sum = 0.0 ifTrue: [ ^0.0 ].
	^sum / ((xStop-xStart+1) * (yStop-yStart+1))! !

!FloatImage methodsFor: 'resampling' stamp: 'jmv 5/22/2015 12:25'!
areaTransformedBy: anAffineTransformation resultExtent: resultExtentOrNil
	"Answer a new image.
	anAffineTransformation specifies the transformation from us to the result image.
	resultExtentOrNil ifNil: [ make it large enough ]

	Warning: there could be black areas in the result, and/or some image content in self could be lost.
	This depends on the arguments.

	Apply a pixel area filtering for interpolation, akin to OpenCV's CV_INTER_AREA.
	Very fast. Very good for strong zoom out (like building image thumbnails).
	"
	| scaled resultHeight resultWidth p v hh hw inverseTransformation e |

	e _ resultExtentOrNil ifNil: [ self resultExtentFor: anAffineTransformation ].
	resultWidth _ e x.
	resultHeight _ e y.
	scaled _ self appropriateResultClass width: resultWidth height: resultHeight.
	inverseTransformation _ anAffineTransformation inverseTransformation.
	hw _ 0.5 * width / resultWidth.
	hh _ 0.5 * height / resultHeight.
	1 to: resultHeight do: [ :y |
		1to: resultWidth do: [ :x |
			p _ inverseTransformation transform: x@y.
			v _ self areaFilteredValueAtX: p x y: p y halfWidth: hw halfHeight: hh.
			scaled x: x y: y put: v 
		]].
	^scaled! !

!FloatImage methodsFor: 'conversion' stamp: 'jmv 7/18/2016 11:46:01'!
asForm
	"Assume the receiver is normalized in [0.0 .. 255/256.0]. Clip values out of range.
	For this reason, don't move this implementation to FloatMatrix, they will most likely use
	other value ranges. See #fullRangeDisplayAt: and #linearDisplayAt:
	
	Consider calling the much faster #asFormQuick
	
	Assume receiver is in gamma space (as a Form, and as jpg or png files)
	(See #linearSpaceToGammaEncoded and #linearSpaceToGammaEncoded)"

	| answer |
	answer _ self as8bitFormEvaluating: [ :index | | value |
		value _ elements at: index.
		"Assume the receiver is normalized in [0.0 .. 255/256.0]
		Multiply by 256, to get values in [0.0 .. 255.0] and then round."
		(value * 256 + 0.5) truncated min: 255 max: 0 ].
	^answer! !

!FloatImage methodsFor: 'conversion' stamp: 'jmv 4/28/2015 11:58'!
asFormG: greenImage b: blueImage
	"Answer a 32bit Form with color values.
	Assume the receiver is normalized in [0.0 .. 0.996].
	Values are simply multiplied by 256.0 and truncated, to produce 8bit pixel values.
	Assume receiver is in gamma space (as a Form, and as jpg or png files)
	(See #linearSpaceToGammaEncoded and #linearSpaceToGammaEncoded)"
	"Warning: No clamping of values out of range. 
	If needed, callers might do something like
		image replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ]"

	| answer |
	answer _ Form extent: width@height depth: 32.
	self fill32BitForm: answer withG: greenImage b: blueImage.
	^answer! !

!FloatImage methodsFor: 'conversion' stamp: 'jmv 7/18/2016 11:44:11'!
asFormQuick
	"Answer a 32bit Form with gray values.
	Assume the receiver is normalized in [0.0 .. 0.996].
	Values are simply multiplied by 256.0 and truncated, to produce 8bit pixel values.
	Assume receiver is in gamma space (as a Form, and as jpg or png files)
	(See #linearSpaceToGammaEncoded and #linearSpaceToGammaEncoded)"
	"Warning: No clamping of values out of range. 
	If needed, callers might call #asForm"

	| answer |
	answer _ GrayForm extent: width@height.
	self fillGrayForm: answer.
	^answer! !

!FloatImage methodsFor: 'conversion' stamp: 'jmv 5/4/2016 11:05'!
asGammaFormUnitRange
	"See comment at #fullRangeDisplayAt:"

	^ self as8bitFormEvaluating: [ :index | | value |
		value _ Color linearTosRGBGamma: ((elements at: index) min: 1.0 max: 0.0).
		(value * 255 + 0.5) truncated ]! !

!FloatImage methodsFor: 'resampling' stamp: 'jmv 5/22/2015 12:25'!
bSplineTransformedBy: anAffineTransformation resultExtent: resultExtentOrNil
	"Answer a new image.
	anAffineTransformation specifies the transformation from us to the result image.
	resultExtentOrNil ifNil: [ make it large enough ]

	Warning: there could be black areas in the result, and/or some image content in self could be lost.
	This depends on the arguments.

	See comment at BSplineInterpolatingImage. Not available in OpenCV.
	Gives top quality results for large scale values (not much smaller than 1.0)
	 In such cases, prefer a filtering technique such as Area or Lanczos to avoid aliasing.
	"
	| scaled interpolator resultHeight resultWidth p v inverseTransformation e |
	e _ resultExtentOrNil ifNil: [ self resultExtentFor: anAffineTransformation ].
	resultWidth _ e x.
	resultHeight _ e y.
	scaled _ self appropriateResultClass width: resultWidth height: resultHeight.	
	interpolator _ BSplineInterpolatingImage from: self.
	inverseTransformation _ anAffineTransformation inverseTransformation.
	1 to: resultHeight do: [ :y |
		1to: resultWidth do: [ :x |
			p _ inverseTransformation transform: x@y.
			v _ interpolator interpolatedValueAtX: p x y: p y.
			scaled x: x y: y put: v ]].
	^scaled! !

!FloatImage methodsFor: 'repair' stamp: 'jmv 8/3/2016 14:31:33'!
badPixelCorrectionMapFor: badPixels
	"Answer a ShortWordMatrix with a correction map for badPixels"

	| correctionMap badPixelsSet x x0 x1 y y0 y1 |
	badPixelsSet _ badPixels asSet.
	correctionMap _ ShortWordMatrix extent: 6@badPixels size.
	badPixels withIndexDo: [ :each :i |
		x _ each x.
		y _ each y.
		x0 _ (x-1 to: 1 by: -1) detect: [ :xx | (badPixelsSet includes: xx@y) not] ifNone: [].
		x1 _ (x+1 to: width) detect: [ :xx | (badPixelsSet includes: xx@y) not] ifNone: [].
		x0 ifNil: [ x0 _ x1 ].
		x1 ifNil: [ x1 _ x0 ].
		y0 _ (y-1 to: 1 by: -1) detect: [ :yy | (badPixelsSet includes: x@yy) not] ifNone: [].
		y1 _ (y+1 to: height) detect: [ :yy | (badPixelsSet includes: x@yy) not] ifNone: [].
		y0 ifNil: [ y0 _ y1 ].
		y1 ifNil: [ y1 _ y0 ].
		correctionMap at: 1@i put: x.
		correctionMap at: 2@i put: y.
		correctionMap at: 3@i put: x0.
		correctionMap at: 4@i put: x1.
		correctionMap at: 5@i put: y0.
		correctionMap at: 6@i put: y1.
	].
	^correctionMap
"
image _ FloatImage lena.

badPixelMask _ FloatImage fromArrayOfArrays: {
	#( 0 0 0 1 0 0 0).
	#( 0 0 1 1 1 0 0).
	#( 0 0 1 1 1 0 0).
	#( 0 1 1 1 1 1 0).
	#( 0 1 1 1 1 1 0).
	#( 1 1 1 1 1 1 0).
	#( 0 1 1 1 1 0 0).
	#( 0 0 1 1 0 0 0) }.

broken _ image copy.	
badPixels _ OrderedCollection new.
{100@93. 90@73. 20@20. 150@40. 30@110. 200@192. 220@120. 220@100. 220@80. 220@60. 220@40. 220@20. 60@160. 120@120. 132@123} do: [ :badPixelsOrigin |
	badPixelMask withIndexesDo: [ :i :j :v |
		v = 1 ifTrue: [
			badPixelPos _ badPixelsOrigin + (j@i) - 1.
			badPixels add: badPixelPos.
			broken at: badPixelPos put: 1.
			]]].

image display.
broken display.

correctionMap _ image badPixelCorrectionMapFor: badPixels.
repaired _ broken copy.
repaired repair: correctionMap.

repaired display
"! !

!FloatImage methodsFor: 'arithmetic-modify self' stamp: 'jmv 8/23/2014 23:40'!
beMagnitudeOfComplexesWith: imaginary
	"Warning. Modifies self to store result. In the process, modifies the argument."

	| iv |
	iv _ imaginary elements.
	elements *= elements.
	iv *= iv.
	elements += iv.
	self replaceValues: [ :i :j :v | v sqrt ]! !

!FloatImage methodsFor: 'centroids' stamp: 'jmv 7/5/2016 14:53'!
bicubicCentroidAround: centroidHint
	| d delta dir dx dy f max xFilter |
	xFilter _ FloatArray new: 4.
	d _ 0.1.	"Distance from hint where we look for max. Should work even if real centroid is much closer to hint."
	"find angle of max growth of interpolator"
	max _ 0.
	0.0 to: Float pi * 2 count: 1000 do: [ :theta |
		dx _ theta cos * d. dy _ theta sin * d.
		f _ self bicubicInterpolatedValueAtX: centroidHint x+dx y: centroidHint y+dy xFilterStore: xFilter.
		f > max ifTrue: [
			max _ f. dir _ dx@dy ]].
	"look for max value of interpolator"
	max _ 0.
	0.0 to: 1.0/d count: 1000 do: [ :step |
		dx _ dir x * step. dy _ dir y * step.
		f _ self bicubicInterpolatedValueAtX: centroidHint x+dx y: centroidHint y+dy xFilterStore: xFilter.
		f > max ifTrue: [
			max _ f. delta _ dx@dy ]].
	^centroidHint + delta! !

!FloatImage methodsFor: 'interpolation' stamp: 'jmv 5/4/2015 13:44'!
bicubicInterpolatedValueAtX: x y: y xFilterStore: xFilter
	"Evaluate a bicubic (i.e. good quality) interpolation
	http://en.wikipedia.org/wiki/Bicubic_interpolation
	http://www.engr.mun.ca/~baxter/Publications/ImageZooming.pdf
	
	Like OpenCV's CV_INTER_CUBIC"

	| interpolated yIndex0 xIndex0 dy dx uy ux xInSource xInSourceClamped yInSource yInSourceClamped dxSquared dySquared |
	xIndex0 _ x floor - 1.

	"x < 0.2 ifTrue: [ ^0.0 ]."			"Do it with integer arithmetic if at all possible"
	xIndex0 < -1 ifTrue: [ ^0.0 ].
	(xIndex0 = -1 and: [x < 0.2]) ifTrue: [ ^0.0 ].
	"x > (width + 0.8) ifTrue: [ ^0.0 ]."	"Do it with integer arithmetic if at all possible"
	xIndex0 > width ifTrue: [ ^0.0 ].
	(xIndex0 = width and: [x > (width + 0.8)]) ifTrue: [ ^0.0 ].


	yIndex0 _ y floor - 1.

	"y < 0.2 ifTrue: [ ^0.0 ]."			"Do it with integer arithmetic if at all possible"
	yIndex0 < -1 ifTrue: [ ^0.0 ].
	(yIndex0 = -1 and: [y < 0.2]) ifTrue: [ ^0.0 ].
	"y > (height + 0.8) ifTrue: [ ^0.0 ]."	"Do it with integer arithmetic if at all possible"
	yIndex0 > height ifTrue: [ ^0.0 ].
	(yIndex0 = height and: [y > (height + 0.8)]) ifTrue: [ ^0.0 ].

	0 to: 3 do: [ :i |
		dx _ (x - (xIndex0 + i)) abs.
		dxSquared _ dx*dx.
		ux _ dx < 2
			ifTrue: [
				dx < 1
					ifTrue: [ (1.5*dx*dxSquared) - (2.5*dxSquared)+1 ]
					ifFalse: [ (-0.5*dx*dxSquared) + (2.5*dxSquared) - (4*dx) + 2 ]]
			ifFalse: [0.0].
		xFilter at: i+1 put: ux ].

	interpolated _ 0.0.
	0 to: 3 do: [ :i |
		dy _ (y - (yIndex0 + i)) abs.
		dySquared _ dy*dy.
		uy _ dy < 2
			ifTrue: [
				dy < 1
					ifTrue: [ (1.5*dy*dySquared) - (2.5*dySquared)+1 ]
					ifFalse: [ (-0.5*dy*dySquared) + (2.5*dySquared) - (4*dy) + 2 ]]
			ifFalse: [0.0].
		
		yInSource _ yIndex0 + i.
		yInSourceClamped _ yInSource min: height max: 1.
		0 to: 3 do: [ :j |
			xInSource _ xIndex0 + j.
			ux _ xFilter at: j+1.
			xInSourceClamped _ xInSource min: width max: 1.
			interpolated _ (self x: xInSourceClamped y: yInSourceClamped) * uy * ux + interpolated ]].
	
	^interpolated! !

!FloatImage methodsFor: 'resampling' stamp: 'jmv 5/22/2015 12:25'!
bicubicTransformedBy: anAffineTransformation resultExtent: resultExtentOrNil
	"Answer a new image.
	anAffineTransformation specifies the transformation from us to the result image.
	resultExtentOrNil ifNil: [ make it large enough ]

	Warning: there could be black areas in the result, and/or some image content in self could be lost.
	This depends on the arguments.

	Evaluate a bicubic (i.e. good quality) interpolation. Like OpenCV's CV_INTER_CUBIC
	"
	| scaled resultHeight resultWidth p v xFilter inverseTransformation e |
	e _ resultExtentOrNil ifNil: [ self resultExtentFor: anAffineTransformation ].
	resultWidth _ e x.
	resultHeight _ e y.
	scaled _ self appropriateResultClass width: resultWidth height: resultHeight.
	inverseTransformation _ anAffineTransformation inverseTransformation.
	xFilter _ FloatArray new: 4.
	1 to: resultHeight do: [ :y |
		1to: resultWidth do: [ :x |
			p _ inverseTransformation transform: x@y.
			v _ self bicubicInterpolatedValueAtX: p x y: p y xFilterStore: xFilter.
			scaled x: x y: y put: v
		]].
	^scaled! !

!FloatImage methodsFor: 'interpolation' stamp: 'jmv 1/21/2016 12:21'!
bilinearInterpolatedClampToEdgeValueAtX: x y: y
	"Bilinear interpolation. If acessed outside bounds, clamp to the edge. (Like OpenCL's  CLK_ADDRESS_CLAMP_TO_EDGE)"

	^ self bilinearInterpolatedValueAtX: (x min: width max: 1) y: (y min: height max: 1)
! !

!FloatImage methodsFor: 'resampling' stamp: 'jmv 5/22/2015 12:25'!
bilinearTransformedBy: anAffineTransformation resultExtent: resultExtentOrNil
	"Answer a new image.
	anAffineTransformation specifies the transformation from us to the result image.
	resultExtentOrNil ifNil: [ make it large enough ]

	Warning: there could be black areas in the result, and/or some image content in self could be lost.
	This depends on the arguments.

	Evaluate a bilinear (i.e. cheap) interpolation.
	Like OpenCV's CV_INTER_LINEAR"

	| scaled resultHeight resultWidth p v inverseTransformation e |
	e _ resultExtentOrNil ifNil: [ self resultExtentFor: anAffineTransformation ].
	resultWidth _ e x.
	resultHeight _ e y.
	scaled _ self appropriateResultClass width: resultWidth height: resultHeight.
	inverseTransformation _ anAffineTransformation inverseTransformation.
	1 to: resultHeight do: [ :y |
		1to: resultWidth do: [ :x |
			p _ inverseTransformation transform: x@y.
			v _ self bilinearInterpolatedValueAtX: p x y: p y.
			scaled x: x y: y put: v
		]].
	^scaled! !

!FloatImage methodsFor: 'metrics' stamp: 'jmv 9/4/2014 14:22'!
blurEstimator
	"Estimate blur"
	| aux eBlur nBlur |
	aux _ FloatImage extent: self extent.
	eBlur _ self blurEstimatorDirection: 1@0 aux: aux.
	nBlur _ self blurEstimatorDirection: 0@1 aux: aux.
	^eBlur @ nBlur! !

!FloatImage methodsFor: 'metrics' stamp: 'jmv 9/8/2014 16:25'!
blurEstimatorDirection: direction aux: auxImage
"aca lo que buscamos es que la derivada del 'accutance' baje a la mitad del primer valor..."
"
Anda bien para blurs de entre 1 y 40 pixels
Nunca iterar mas de 100 veces, ni contestar valores mayores que 100
FloatImage blurEstimationExperiment
"
	| butLastValue lastValue delta derivative firstValueOfDerivative stepCount |
	delta _ direction.
	butLastValue _ self acutanceAt: delta aux: auxImage.
	delta _ delta + direction. 
	lastValue _ self acutanceAt: delta aux: auxImage.
	derivative _ lastValue - butLastValue.
	firstValueOfDerivative _ derivative.
	firstValueOfDerivative < 1.2e-4 ifTrue: [ ^ 100 ].
	delta _ delta + direction. 
	stepCount _ 0.
	[ derivative > (firstValueOfDerivative / 2) and: [stepCount < 100]] whileTrue: [
		butLastValue _ lastValue.
		lastValue _ self acutanceAt: delta aux: auxImage.
		derivative _ lastValue - butLastValue.
		stepCount _ stepCount + 1.
		delta _ delta + direction. ].
	^stepCount! !

!FloatImage methodsFor: 'centroids' stamp: 'jmv 7/5/2016 14:44'!
boxAbove: threshold atX: x y: y
	"Find the smallest box (a rectangular area) that includes x@y and is above threshold.
	More precisely, start with x@y and add additional rows and columns around it as long as they include some value above threshold.
	The result has a one-sample border without values above threshold, and all inner rows and columns have at least one sample above it.
	The idea is to build the smallest possible box that includes a 'black' border.
	Note: in any case, stop at image edges."
	| continue x0 x1 y0 y1 |
	x0 _ x. x1 _ x.
	y0 _ y. y1 _ y.
	continue _ true.
	[ continue ] whileTrue: [
		continue _ false.
		[y0 > 1 and: [(x0 to: x1) anySatisfy: [ :xx | (self x: xx y: y0) > threshold ]]] whileTrue: [
			y0 _ y0-1.
			continue _ true ].
		[y1 < self height and: [(x0 to: x1) anySatisfy: [ :xx | (self x: xx y: y1) > threshold ]]] whileTrue: [
			y1 _ y1+1.
			continue _ true ].
		[x0 > 1 and: [(y0 to: y1) anySatisfy: [ :yy | (self x: x0 y: yy) > threshold ]]] whileTrue: [
			x0 _ x0-1.
			continue _ true ].
		[x1 < self width and: [(y0 to: y1) anySatisfy: [ :yy | (self x: x1 y: yy) > threshold ]]] whileTrue: [
			x1 _ x1+1.
			continue _ true ].
	].
	^x0@y0 corner: x1@y1+1! !

!FloatImage methodsFor: 'centroids' stamp: 'jmv 7/5/2016 14:53'!
bsplineCentroidAround: centroidHint
	| d delta dir dx dy f max interpolator |
	interpolator _ BSplineInterpolatingImage from: self.
	d _ 0.1.	"Distance from hint where we look for max. Should work even if real centroid is much closer to hint."
	"find angle of max growth of interpolator"
	max _ 0.
	0.0 to: Float pi * 2 count: 1000 do: [ :theta |
		dx _ theta cos * d. dy _ theta sin * d.
		f _ interpolator interpolatedValueAtX: centroidHint x+dx y: centroidHint y+dy.
		f > max ifTrue: [
			max _ f. dir _ dx@dy ]].
	"look for max value of interpolator"
	max _ 0.
	0.0 to: 1.0/d count: 1000 do: [ :step |
		dx _ dir x * step. dy _ dir y * step.
		f _ interpolator interpolatedValueAtX: centroidHint x+dx y: centroidHint y+dy.
		f > max ifTrue: [
			max _ f. delta _ dx@dy ]].
	^centroidHint + delta.! !

!FloatImage methodsFor: 'centroids' stamp: 'jmv 7/5/2016 14:45'!
centerOfMass
	""
	| star2 starCenterOfMassX starCenterOfMassY |
	star2 _ self copy normalizeSum.
	star2 replaceValues:  [ :i :j :v | v * i ].
	starCenterOfMassY _ star2 sum.
	star2 _ self copy normalizeSum.
	star2 replaceValues:  [ :i :j :v | v * j ].
	starCenterOfMassX _ star2 sum.
	^starCenterOfMassX@starCenterOfMassY.! !

!FloatImage methodsFor: 'centroids' stamp: 'jmv 7/5/2016 14:44'!
centroidAround4: aPoint
	"Find the centroid of the spot that covers aPoint. aPoint must contain the local maximum.
	Use given point, and 4 neighbors.
	See #testCentroids

	| i |
	i _ FloatImage width: 5 height: 5.
	i x: 3 y: 3 put: 1.
	i x: 4 y: 3 put: 1.
	(i centroidAround4: 3@3) print
	
	Resulta ser equivalente a
	https://ccrma.stanford.edu/~jos/parshl/Peak_Detection_Steps_3.html
	O sea que tan loco no estoy!!
	Pero solo anda bien para lobulos grandes, donde tenga 3 muestras positivas.
	O sea alta correlacion para desplazamientos de hasta un pixel.
	Una alternativa podria ser sobremuestrear las imagenes, usando interpolacion spline (mhhh. o quizas incluso lanczos). Asi, pediria correlacion positiva para desplazamientos de hasta medio pixel, lo que suena mas razonable...
	Sigo con esto en centroidAround: y centroidAround5:
	"

	| centroidX centroidY dx0 dx2 dy0 dy2 fx0 fx2 fy0 fy2 localMax x0 x1 x2 y0 y1 y2 |
	"x1@y1 contain the max value. x0, x2, y0, y2 are neighbors, but wrap around on edges"
	x1 _ aPoint x.
	x0 _ x1 - 1.
	x2 _ x1 + 1.
	x0 = 0
		ifTrue: [ x0 _ width ]
		ifFalse: [
			x2 = (width+1)
				ifTrue: [ x2 _ 1 ]].
	y1 _ aPoint y.
	y0 _ y1 - 1.
	y2 _ y1 + 1.
	y0 = 0
		ifTrue: [ y0 _ height ]
		ifFalse: [
			y2 = (height+1)
				ifTrue: [ y2 _ 1 ]].

	localMax _ self x: x1 y: y1.

	dx0 _ localMax - (self x: x0 y: y1).
	dx2 _ localMax - (self x: x2 y: y1).
	fx0 _ dx2 / (dx0 + dx2) / 2 + 0.25.
	fx2 _ dx0 / (dx0 + dx2) / 2 + 0.25.
	centroidX _ x1 - fx0 + fx2.

	dy0 _ localMax - (self x: x1 y: y0).
	dy2 _ localMax - (self x: x1 y: y2).
	fy0 _ dy2 / (dy0 + dy2) / 2 + 0.25.
	fy2 _ dy0 / (dy0 + dy2) / 2 + 0.25.
	centroidY _ y1 - fy0 + fy2.

	^centroidX @ centroidY! !

!FloatImage methodsFor: 'centroids' stamp: 'jmv 7/5/2016 14:44'!
centroidAround: aPoint
	"Find the centroid of the spot that covers aPoint. aPoint must contain the local maximum.
	See #testCentroids
	| i |
	i _ FloatImage width: 5 height: 5.
	i x: 3 y: 3 put: 1.
	i x: 4 y: 3 put: 1.
	(i centroidAround: 3@3) print
	
	Based on 
	https://ccrma.stanford.edu/~jos/parshl/Peak_Detection_Steps_3.html
	"

	| centroidX centroidY beta x0 x1 x2 y0 y1 y2 alpha gamma crazyFactor |
	"x1@y1 contain the max value. x0, x2, y0, y2 are neighbors, but wrap around on edges"
	x1 _ aPoint x.
	x0 _ x1 - 1.
	x2 _ x1 + 1.
	x0 = 0
		ifTrue: [ x0 _ width ]
		ifFalse: [
			x2 = (width+1)
				ifTrue: [ x2 _ 1 ]].
	y1 _ aPoint y.
	y0 _ y1 - 1.
	y2 _ y1 + 1.
	y0 = 0
		ifTrue: [ y0 _ height ]
		ifFalse: [
			y2 = (height+1)
				ifTrue: [ y2 _ 1 ]].

"Este valor fue encontrado usando #testCentroids
Hace que los lobulos negativos de una sinc se parezcan un poco a una parábola."
crazyFactor _ 1.8892666638.

"Asi optimiza autoCorregistraction7.txt ... Pero parece una exageracion!!"
"crazyFactor _ 7.6."

"La conclusion seria que el remuestreo de una imagen raw genera algo que no es exactamente una sinc..."

	alpha _ self x: x0 y: y1.
	beta _ self x: x1 y: y1.
	gamma _ self x: x2 y: y1.

	alpha < 0 ifTrue: [ alpha _ alpha * crazyFactor ].
	gamma < 0 ifTrue: [ gamma _ gamma * crazyFactor ].
	centroidX _ alpha-gamma / (alpha - (beta * 2) + gamma) / 2 + x1.

	alpha _ self x: x1 y: y0.
	beta _ self x: x1 y: y1.
	gamma _ self x: x1 y: y2.

	alpha < 0 ifTrue: [ alpha _ alpha * crazyFactor ].
	gamma < 0 ifTrue: [ gamma _ gamma * crazyFactor ].
	centroidY _ alpha-gamma / (alpha - (beta * 2) + gamma) / 2 + y1.

	^centroidX @ centroidY! !

!FloatImage methodsFor: 'copying' stamp: 'jmv 5/22/2015 12:25'!
cheapDisplacedBy: deltaPoint
	"Answer a new image our contents, sort of displaced by aPoint.
	Not really precise. Shift in the X direction wrap around the next or previous line!!
			displacement is integer!!"
	
	| answer |
	answer _ self appropriateResultClass width: width height: height.
	answer fillWith: self cheaplyDisplaced: deltaPoint.
	^answer! !

!FloatImage methodsFor: 'filtering' stamp: 'jmv 5/22/2015 13:08'!
convolutionLinearWith: aFloatMatrix
	"Linear convolution. Warning: Slow.
	Answer is of enough size to hold the whole convolution
	It is usually preferred to do a circular convolution via FFT and take care of border effects.
	But this one might be preferred if the receiver is big, and the argument is very small
	Answer a new FloatImage. Don't modify receiver or argument.
	
	Performance could be enhanced using the fast #fillWith:displaced: on a temporal image, for each nonzero element of the kernel,
	and multiply and add with fast floatArray operations"

	| answer h w v |
	h _ aFloatMatrix height.
	w _ aFloatMatrix width.
	answer _ self appropriateResultClass width: width + aFloatMatrix width - 1 height: height + aFloatMatrix height - 1.
	1 to: height do: [ :y |
		1 to: width do: [ :x |
			v _ self x: x y: y.
			1 to: h do: [ :i |
				1 to: w do: [ :j |
					answer
						x: x + j-1 y: y + i-1
						put: v * (aFloatMatrix i: i j: j) + (answer x: x + j-1 y: y + i-1)
					]]]].
	^answer! !

!FloatImage methodsFor: 'filtering' stamp: 'jmv 5/22/2015 13:08'!
convolutionLinearWithSmallKernel: kernel
	"Linear convolution. Warning: Slow.
	Answer is same extent as receiver.
	It is usually preferred to do a circular convolution via FFT and take care of border effects.
	But this one might be preferred if the receiver is big, and the argument is very small
	Answer a new FloatImage. Don't modify receiver or argument.

	Performance could be enhanced using the fast #fillWith:displaced: on a temporal image, for each nonzero element of the kernel,
	and multiply and add with fast floatArray operations"

	| answer kernelH kernelW v deltaI deltaJ iMax iMin jMax jMin xx yy |
	kernelH _ kernel height.
	kernelW _ kernel width.
	deltaI _ kernelH // 2 + 1.
	deltaJ _ kernelW // 2 + 1.
	
	answer _ self appropriateResultClass width: width height: height.
	
	1 to: height do: [ :y |
		iMin _ 1 max: y+deltaI-height.
		iMax _ kernelH min: y+deltaI-1.
		
		1 to: width do: [ :x |
			jMin _ 1 max: x+deltaJ-width.
			jMax _ kernelW min: x+deltaJ-1.

			v _ 0.0.	
			iMin to: iMax do: [ :i |
				yy _ y-i+deltaI.
				jMin to: jMax do: [ :j |
					xx _ x-j+deltaJ.
					v _ (self x: xx y: yy) * (kernel i: i j: j) + v ]].
			answer x: x y: y put: v ]].
	^answer! !

!FloatImage methodsFor: 'filtering' stamp: 'jmv 9/29/2017 16:40:32'!
convolutionWith: trimmedPsf
	"Compute convolution with aMatrix, usig 2D FFT. Answer a new instance.
	- pixelScaledPsf is at the same scale as receiver, but usually much smaller. Largest coefficients should be at the center of the image
		(as image filters are usually specified)
	- The convolution is computed in the complex DFT space. Input is assumed to be real, and answer is real too.
	- This method uses a a Circular Convolution, but the edges are trimmed to remove border effects.
	- Implemented with tiles. Tile border effects are taken care of.
	
	"

	^ self convolutionWith: trimmedPsf trimResult: true guardZoneInto: [ :guardZoneHeight : guardZoneWidth | ]! !

!FloatImage methodsFor: 'filtering' stamp: 'jmv 9/29/2017 16:39:05'!
convolutionWith: trimmedPsf trimResult: trimResult guardZoneInto: aBlock
	"Compute convolution with aMatrix, usig 2D FFT. Answer a new instance.
	- pixelScaledPsf is at the same scale as receiver, but usually much smaller. Largest coefficients should be at the center of the image
		(as image filters are usually specified)
	- The convolution is computed in the complex DFT space. Input is assumed to be real, and answer is real too.
	- This method uses a a Circular Convolution, but the edges are trimmed to remove border effects.
	- Implemented with tiles. Tile border effects are taken care of.

	Result image is smaller. Guard zones are used to avoid border effects in each tile, but also to avoid them in the result image. If trimResult is false, then keep original size, but leave a black border at the edges, covering the result guard zone.
	"

	| tileSize otfRealPart otfImaginaryPart fft tileImaginary guardZoneHeight guardZoneWidth |
	tileSize _ 1024 min: (2 raisedToInteger: (width max: height) log2 ceiling).

	"Prepare convolution kernel for the actual extent of the tile."
	fft _ self otfForPsf: trimmedPsf heightAndWidth: tileSize.
	otfRealPart _ fft realData.
	otfImaginaryPart _ fft imagData.

	"Do convolution in each tile and get results."
	guardZoneWidth _ (trimmedPsf width / 2) ceiling.		"Number of pixels we are willing to lose on each side, to avoid border effects."
	guardZoneHeight _ (trimmedPsf height / 2) ceiling.		"Number of pixels we are willing to lose at top and bottom, to avoid border effects."
	aBlock value: guardZoneHeight value: guardZoneWidth.
	tileImaginary _ FloatImage width: tileSize height: tileSize.
	^ self
		withTilesDo: [ :tileReal |
			tileImaginary *= 0.
			"Transform to Fourier space"
			fft realData: tileReal imagData: tileImaginary.
			fft transformForward: true.
			"Multiply."
			tileReal imaginary: tileImaginary elementMultiplyBy: otfRealPart imaginary: otfImaginaryPart.
			"And transform back"
			fft transformForward: false ] 
		tileSize: tileSize
		horizontalGuardZone: guardZoneWidth
		verticalGuardZone: guardZoneHeight
		trimResult: trimResult! !

!FloatImage methodsFor: 'image registration' stamp: 'jmv 6/30/2017 11:58:11'!
correlationABSWith: another deltai: di deltaj: dj
	"Quite like #correlationWith:deltai:deltaj: Please understand it before this.
	
	Usually we just want #correlationWith:deltai:deltaj: . Sometimes we have images with negative correlation, and that case is also handled well. The problem is with images that might have positive correlation in parts of it, and negative correlation in other parts of it. This method tries to adress that need. It will give values between 0.0 (no correlation) and 1.0 (maximum similarity), regardless of correlation being positive or negatice. 
	
	Play with the examples (and understand the nature of the results)!!

	| i1 i2 |
	i1 _ FloatImage lena.
	i2 _ i1 copy: (31@21 corner: i1 extent).
	i2 _ i2 * 0.7.
	i1 addNormalNoise: 0.02.
	i2 addNormalNoise: 0.05.
	{ i1 correlationWith: i2 deltai: 20 deltaj: 30. i1 correlationWith: i2 deltai: 20 deltaj: 31 } print.
	{ i1 correlationABSWith: i2 deltai: 20 deltaj: 30. i1 correlationABSWith: i2 deltai: 20 deltaj: 31 } print.
	i1 displayAt: 0@0.
	i2 displayAt: 256@0.
	nil.

	| i1 i2 |
	i1 _ FloatImage lena.
	i2 _ i1 copy: (31@21 corner: i1 extent).
	i2 _ i2 * -0.7 + 1.
	i1 addNormalNoise: 0.02.
	i2 addNormalNoise: 0.05.
	{ i1 correlationWith: i2 deltai: 20 deltaj: 30. i1 correlationWith: i2 deltai: 20 deltaj: 31 } print.
	{ i1 correlationABSWith: i2 deltai: 20 deltaj: 30. i1 correlationABSWith: i2 deltai: 20 deltaj: 31 } print.
	i1 displayAt: 0@0.
	i2 displayAt: 256@0.
	nil.

	| i1 i2 |
	i1 _ FloatImage lena.
	i2 _ i1 copy: (31@21 corner: i1 extent).
	i2 replaceValues: [:i :j :v | i > 100 ifTrue: [v* -0.9 + 0.7] ifFalse: [v* 0.7]].
	i1 addNormalNoise: 0.02.
	i2 addNormalNoise: 0.05.
	{ i1 correlationWith: i2 deltai: 20 deltaj: 30. i1 correlationWith: i2 deltai: 20 deltaj: 31 } print.
	{ i1 correlationABSWith: i2 deltai: 20 deltaj: 30. i1 correlationABSWith: i2 deltai: 20 deltaj: 31 } print.
	i1 displayAt: 0@0.
	i2 displayAt: 256@0.
	nil.
	"

	| sum mySquaredSum othersSquaredSum mine others iEnd iStart jEnd jStart othersMean myMean count |
	iStart _ 1 max: 1+di.
	iEnd _ height min: another height+di.
	jStart _ 1 max: 1+dj.
	jEnd _ width min: another width+dj.

	"Compute average for both images, but include only those pixels that will be used later for computing correlation"
	myMean _ 0.0.
	othersMean _ 0.0.
	count _ 0.
	iStart to: iEnd do: [ :i |
		jStart to: jEnd do: [ :j |
			mine _ self i: i j: j.
			others _ another i: i-di j: j-dj.
			mine * others = 0.0 ifFalse: [
				myMean _ mine + myMean.
				othersMean _ others + othersMean.
				count _ count + 1 ]]].
	myMean _ myMean / count.
	othersMean _ othersMean / count.

	"Compute image correlation. Use the same pixels as before."
	sum _ 0.0.
	mySquaredSum _ 0.0.
	othersSquaredSum _ 0.0.
	iStart to: iEnd do: [ :i |
		jStart to: jEnd do: [ :j |
			mine _ self i: i j: j.
			others _ another i: i-di j: j-dj.
			mine * others = 0.0 ifFalse: [
				mine _ mine - myMean.
				others _ others - othersMean.
				sum _ (mine * others) abs + sum.										"This is the difference!!"
				mySquaredSum _ mine squared + mySquaredSum.
				othersSquaredSum _ others squared + othersSquaredSum ]]].
	sum = 0.0 ifTrue: [ ^ 0.0 ].
	^ sum / (mySquaredSum * othersSquaredSum) sqrt! !

!FloatImage methodsFor: 'image registration' stamp: 'jmv 9/29/2017 15:38:20'!
correlationWith: another deltai: di deltaj: dj
	"https://en.wikipedia.org/wiki/Digital_image_correlation

	Ignore any pixels with zero value at any of the images: consider them as 'no data'.
	This is to avoid the risk of partially overlapping images (maybe zero-padded, or with value zero outside actual image data)
	to give maximum correlation at distances with maximum overlap, and not with matching features.

	| i i2 |
	i _ FloatImage lena.
	i2 _ i copy: (31@21 corner: i extent).
	i2 _ i2 * 0.7.
	i addNormalNoise: 0.02.
	i2 addNormalNoise: 0.05.
	{ i correlationWith: i2 deltai: 20 deltaj: 30.
	i correlationWith: i2 deltai: 20 deltaj: 31 }

	| i i2 |
	i _ FloatImage lena.
	i2 _ i copy: (31@21 corner: i extent).
	i2 _ i2 * -0.7.
	i addNormalNoise: 0.02.
	i2 addNormalNoise: 0.05.
	{ i correlationWith: i2 deltai: 20 deltaj: 30.
	i correlationWith: i2 deltai: 20 deltaj: 31 }
	"

	| sum mySquaredSum othersSquaredSum mine others iEnd iStart jEnd jStart othersMean myMean count |
	iStart _ 1 max: 1+di.
	iEnd _ height min: another height+di.
	jStart _ 1 max: 1+dj.
	jEnd _ width min: another width+dj.

	"Compute average for both images, but include only those pixels that will be used later for computing correlation"
	myMean _ 0.0.
	othersMean _ 0.0.
	count _ 0.
	iStart to: iEnd do: [ :i |
		jStart to: jEnd do: [ :j |
			mine _ self i: i j: j.
			others _ another i: i-di j: j-dj.
			mine * others = 0.0 ifFalse: [
				myMean _ mine + myMean.
				othersMean _ others + othersMean.
				count _ count + 1 ]]].
	count = 0 ifTrue: [ ^ 0.0 ].
	myMean _ myMean / count.
	othersMean _ othersMean / count.

	"Compute image correlation. Use the same pixels as before."
	sum _ 0.0.
	mySquaredSum _ 0.0.
	othersSquaredSum _ 0.0.
	iStart to: iEnd do: [ :i |
		jStart to: jEnd do: [ :j |
			mine _ self i: i j: j.
			others _ another i: i-di j: j-dj.
			mine * others = 0.0 ifFalse: [
				mine _ mine - myMean.
				others _ others - othersMean.
				sum _ mine * others + sum.
				mySquaredSum _ mine squared + mySquaredSum.
				othersSquaredSum _ others squared + othersSquaredSum ]]].
	sum = 0.0 ifTrue: [ ^ 0.0 ].
	^ sum / (mySquaredSum * othersSquaredSum) sqrt! !

!FloatImage methodsFor: 'image registration' stamp: 'jmv 10/17/2016 10:39:31'!
crossCorrelationWith: aFloatImage phase: aBoolean
	"Compute cross correlation with argument, usig 2D FFT. Store the result on self.
	if aBoolean, compute phase correlation.

	See http://en.wikipedia.org/wiki/Cross_correlation
	See http://en.wikipedia.org/wiki/Phase_correlation
	See http://en.wikipedia.org/wiki/Image_registration

	Note: Circular convolution (input is considered periodic, repeating itself at ends).
	Note: Input is assumed to be real, and answer is real too.
	Note: modifies both self and aFloatImage. Call with copies if appropriate.
	"
	| imaginary argumentImaginary fft a b c d e f magnitude |
	imaginary _ FloatImage width: width height: height.
	argumentImaginary _ FloatImage width: width height: height.

	"Transform both to Fourier space. Allocate space for imaginary coefficients."
	fft _ FFT2D new.
	fft realData: aFloatImage imagData: argumentImaginary.
	fft transformForward: true.
	fft realData: self imagData: imaginary.
	fft transformForward: true.

	"Use complex conjugate of argument"
	argumentImaginary *= -1.
"imaginary *= -1."

	"Multiply"
	1 to: height do: [ :y |
		1 to: width do: [ :x |
			a _ self x: x y: y.
			b _ imaginary x: x y: y.
			c _ aFloatImage x: x y: y.
			d _ argumentImaginary x: x y: y.
			e _ a * c - (b * d).
			f _ b * c + (a * d).
			aBoolean ifTrue: [
				magnitude _ (e squared + f squared) sqrt.
				magnitude = 0.0 ifFalse: [
					e _ e / magnitude.
					f _ f / magnitude ]].
			self x: x y: y put: e.
			imaginary x: x y: y put: f ]].

	"And transform back self"
	fft transformForward: false! !

!FloatImage methodsFor: 'filtering' stamp: 'jmv 9/29/2017 16:40:51'!
deconvolutionWith: trimmedPsf maxNoiseAmplification: maxNoiseFactor
	"Compute convolution with aMatrix, usig 2D FFT. Answer a new instance.
	- pixelScaledPsf is at the same scale as receiver, but usually much smaller. Largest coefficients should be at the center of the image
		(as image filters are usually specified)
	- The convolution is computed in the complex DFT space. Input is assumed to be real, and answer is real too.
	- This method uses a a Circular Convolution, but the edges are trimmed to remove border effects.
	- Implemented with tiles. Tile border effects are taken care of.
	- maxNoiseFactor is required in many case to avoid division by zero. Typical values are between 4 and 8. Use values larger than 20 if you know what you are doing.

Play with this in a Workspace
i _ FloatImage lena.
psf _ FloatMatrix withRows: #(
		#(0.8 0.0 0.2) 
		#(0.0 0.1 0.2) 
		#(0.20 0.0 0.0)
	).
psf normalizeSum.
convolved _ i convolutionWith: psf.
noisy _ convolved copy addNormalNoise: 0.05.
deconvolved _ noisy deconvolutionWith: psf maxNoiseAmplification: 13.

i displayAt: 10@10.
convolved displayAt: 10@10.
noisy displayAt: 10@10.
deconvolved displayAt: 10@10.
	"

	^ self deconvolutionWith: trimmedPsf maxNoiseAmplification: maxNoiseFactor trimResult: true guardZoneInto: [ :guardZoneHeight : guardZoneWidth | ]! !

!FloatImage methodsFor: 'filtering' stamp: 'jmv 9/29/2017 16:39:38'!
deconvolutionWith: trimmedPsf maxNoiseAmplification: maxNoiseFactor trimResult: trimResult guardZoneInto: aBlock
	"Compute convolution with aMatrix, usig 2D FFT. Answer a new instance.
	- pixelScaledPsf is at the same scale as receiver, but usually much smaller. Largest coefficients should be at the center of the image
		(as image filters are usually specified)
	- The convolution is computed in the complex DFT space. Input is assumed to be real, and answer is real too.
	- This method uses a a Circular Convolution, but the edges are trimmed to remove border effects.
	- Implemented with tiles. Tile border effects are taken care of.
	- maxNoiseFactor is required in many case to avoid division by zero. Typical values are between 4 and 8. Use values larger than 20 if you know what you are doing.

	Result image is smaller. Guard zones are used to avoid border effects in each tile, but also to avoid them in the result image. If trimResult is false, then keep original size, but leave a black border at the edges, covering the result guard zone.

Play with this in a Workspace
i _ FloatImage lena.
psf _ FloatMatrix withRows: #(
		#(0.8 0.0 0.2) 
		#(0.0 0.1 0.2) 
		#(0.20 0.0 0.0)
	).
psf normalizeSum.
convolved _ i convolutionWith: psf trimResult: true.
noisy _ convolved copy addNormalNoise: 0.05.
deconvolved _ noisy deconvolutionWith: psf maxNoiseAmplification: 13 trimResult: true.
i display.
convolved display.
noisy display.
deconvolved display.

Play with this in a Workspace
i _ FloatImage lena.
psf _ FloatMatrix withRows: #(
		#(0.8 0.0 0.2) 
		#(0.0 0.1 0.2) 
		#(0.20 0.0 0.0)
	).
psf normalizeSum.
convolved _ i convolutionWith: psf trimResult: false.
noisy _ convolved copy addNormalNoise: 0.05.
deconvolved _ noisy deconvolutionWith: psf maxNoiseAmplification: 13 trimResult: false.
i display.
convolved display.
noisy display.
deconvolved display.
	"

	| tileSize otfRealPart otfImaginaryPart fft tileImaginary guardZoneHeight guardZoneWidth otfInverseImaginaryPart otfInverseRealPart psfInverseImaginaryPart psfInverseRealPart trimmed |

	tileSize _ 1024 min: (2 raisedToInteger: (width max: height) log2 ceiling).

	"Prepare convolution kernel for the actual extent of the tile."
	fft _ self otfForPsf: trimmedPsf heightAndWidth: tileSize.
	otfRealPart _ fft realData.
	otfImaginaryPart _ fft imagData.

	"Compute the 'inverse' otf, limiting noise amplification"
	otfInverseRealPart _ FloatImage width: tileSize height: tileSize.
	otfInverseImaginaryPart _ FloatImage width: tileSize height: tileSize.
	otfInverseRealPart += 1.
	otfInverseRealPart imaginary: otfInverseImaginaryPart elementDivideBy: otfRealPart imaginary: otfImaginaryPart maxGain: maxNoiseFactor.

	"Compute the 'inverse' psf, to check its support, and find proper guard zones"
	psfInverseRealPart _ otfInverseRealPart copy.
	psfInverseImaginaryPart _ otfInverseImaginaryPart copy.

"Plot in the Fourier domain. Enable to understand better how these animals behave."
"
trimmed _ psfInverseRealPart moveOriginToCenter: false.
rrf _ trimmed i: trimmed height / 2*0 + 1.
trimmedI _ psfInverseImaginaryPart moveOriginToCenter: false.
iif _ trimmedI i: trimmed height / 2*0 + 1.
g _ FunctionGraphMorph new.
g domain: (1 to: 256).
g addFunction: [ :i | rrf at: i ] color: Color green.
g addFunction: [ :i | iif at: i ] color: Color red.
g addFunction: [ :i | ((rrf at: i) squared + (iif at: i) squared)sqrt ] color: Color blue.
(g embeddedInMorphicWindowLabeled: 'graph') openInWorld.
"

	fft realData: psfInverseRealPart imagData: psfInverseImaginaryPart.
	fft transformForward: false.

"Plot in the spatial domain. Enable to understand better how these animals behave."
"
trimmed _ psfInverseRealPart moveOriginToCenter: false.
rr _ trimmed i: trimmed height / 2*0 + 1.
trimmedI _ psfInverseImaginaryPart moveOriginToCenter: false.
ii _ trimmedI i: trimmed height / 2*0 + 1.
g _ FunctionGraphMorph new.
g domain: (1 to: 256).
g addFunction: [ :i | rr at: i ] color: Color green.
g addFunction: [ :i | ii at: i ] color: Color red.
g addFunction: [ :i | ((rr at: i) squared + (ii at: i) squared)sqrt ] color: Color blue.
(g embeddedInMorphicWindowLabeled: 'graph') openInWorld.
"

	trimmed _ psfInverseRealPart moveOriginToCenter: false.

"Difficult balance between artifacts and lost pixels!!"
"	trimmed _ trimmed trimCentered: trimmed sum / 512."
	"For convolution, we chose sum/512. Support is usually very small and all values are positive (their effect accumulates).
	But deconvolution (inverse) kernels tend to have larger support, and both positive and negative coefficients (whose effects sorts of cancels when coefficients gets smaller to the sides)"
	"trimmed _ trimmed trimCentered: trimmed sum / 64."
	"Maybe should be a parameter. For very large images, this seems appropriate."
	trimmed _ trimmed trimCentered: trimmed sum / 1024.
	"Number of pixels we are willing to lose on each side, to avoid border effects."
	guardZoneWidth _ trimmed width / 2.
	guardZoneHeight _ trimmed height / 2.
	aBlock value: guardZoneHeight value: guardZoneWidth.

	"Do convolution in each tile and get results."
	tileImaginary _ FloatImage width: tileSize height: tileSize.
	^ self
		withTilesDo: [ :tileReal |
			tileImaginary *= 0.
			"Transform to Fourier space"
			fft realData: tileReal imagData: tileImaginary.
			fft transformForward: true.
			"Multiply."
"			tileReal imaginary: tileImaginary elementDivideBy: otfRealPart imaginary: otfImaginaryPart maxGain: maxNoiseFactor."
			tileReal imaginary: tileImaginary elementMultiplyBy: otfInverseRealPart imaginary: otfInverseImaginaryPart.
			"And transform back"
			fft transformForward: false ] 
		tileSize: tileSize
		horizontalGuardZone: guardZoneWidth
		verticalGuardZone: guardZoneHeight
		trimResult: trimResult! !

!FloatImage methodsFor: 'image registration' stamp: 'jmv 11/30/2016 10:51:36'!
deltaToAlign: aFloatImage imagesAreRaw: imagesAreRaw rawDelta: rawDeltaPointOrNil
	"Compute the image registration between self and anImage. Answer the delta aFloatImage should be moved to align perfectly with self.
	Assume there is no scale change and no rotation.
	It is required that both images are of the same size, and that width and height are an integer power of two (to ease FFT).
	It is also required that there is a significant overlap between both images, and that they are not dominated by horizontal or vertical stripes of the same illumination level.

	imagesAreRaw indicates whether receiver and argument are pure raw sensor images, or they have been compensated for bias and dark frame.
	If imagesAreRaw is true, then images should come from the same sensor, at the same temperature, and similar exposure time. In addition to this, images should be cropped (or not) idetically, meaning that the hot pixels, or any other dark frame anomaly shows exactly at the same pixel position in both images. Additionally, there must be a non zero displacement between content in both images. Two images of exactly the same area, or two copies of the same image will fail.
	
	If imagesAreRaw is false, then it is assumed that there are no hot pixels, or any ther dark frame anomaly. Therefore, it is acceptable that images come from crops of different parts of the sensor.

	doPhaseCorrelation ifTrue: [ do phase correlation ] ifFalse: [ do cross correlation ].
	
	Makes copies of both image. Receiver and argument are not modified."
	
	| correlation target x y dx dy c xy doPhaseCorrelation rawPatternPosition h w |

	(width isPowerOfTwo and: [ height isPowerOfTwo ]) 
		ifTrue: [
			correlation _ self copy.
			target _ aFloatImage copy ]
		ifFalse: [ | r |
			w _ 2 raisedToInteger: width log2 floor.
			h _ 2 raisedToInteger: height log2 floor.
			r _ Rectangle center: width@height//2 extent: w@h.
			correlation _ self copy: r.
			target _ aFloatImage copy: r ].

	"Do Phase Correlation. This normalizes the cross correlation result, meaning it makes sense to ask the max value to be above some minimum to be meaningful."
	doPhaseCorrelation _ true.
	correlation crossCorrelationWith: target phase: doPhaseCorrelation.

	"If images are Raw there will be a spurious local maximum at 1@1, or at whatever distance the two images where taken from the original raws.
	This is due to usual sensor pixel gain non uniformity, hot & dead pixels, etc."
	imagesAreRaw ifTrue: [
		rawPatternPosition _ (rawDeltaPointOrNil ifNotNil: [rawDeltaPointOrNil rounded] ifNil: [0@0])+1.
		correlation wrapI: rawPatternPosition y j: rawPatternPosition x put: 0.
		correlation wrapI: rawPatternPosition y+1 j: rawPatternPosition x put: 0.
		correlation wrapI: rawPatternPosition y-1 j: rawPatternPosition x put: 0.
		correlation wrapI: rawPatternPosition y j: rawPatternPosition x+1 put: 0.
		correlation wrapI: rawPatternPosition y j: rawPatternPosition x-1 put: 0 ].

	"Find max"
	xy _ correlation xyForMax.
	x _ xy x.
	y _ xy y.

	"Solo considerar valido el maximo si es mayor que 0.03. Suponemos phase = true. ver 
	An FFT-Based Technique for Translation, Rotation, and Scale-Invariant Image Registration"
	(doPhaseCorrelation and: [ (correlation x: x y: y) < 0.02 ]) ifTrue: [
		self error: 'The displacement between images could not be found' ].

	"Find centroid around x@y"
	c _ correlation centroidAround: x@y.
	dx _ c x-1.
	dy _ c y-1.
	dx > (width//2) ifTrue: [ dx _ dx - width ].
	dy > (height//2) ifTrue: [ dy _ dy - height ].

	^dx @dy! !

!FloatImage methodsFor: 'displaying' stamp: 'jmv 7/18/2016 10:16:43'!
displayAt: aPoint
	"Assume the receiver is normalized in [0.0 .. 1.0]. Clip values out of range.
	For this reason, don't move this implementation to FloatMatrix, they will most likely use
	other value ranges. See #fullRangeDisplayAt: and #linearDisplayAt:
	
	Assume receiver is in gamma space (as a Form, and as jpg or png files) and there's no need to apply gamma.
	(See #linearSpaceToGammaEncoded and #linearSpaceToGammaEncoded)"

	self asForm displayAt: aPoint! !

!FloatImage methodsFor: 'displaying' stamp: 'jmv 8/24/2017 11:38:28'!
displayAt: aPoint zoom: anInteger
	"Uses Nearest Neighbor interpolation, to help see each pixel. Usually useful to view very small images, such as PSFs
	
	FloatImage lena displayAt: 10@10 zoom: 4
	(FloatImage lena copy: (120@120 extent: 30@30)) displayAt: 10@10 zoom: 15
	(FloatImage lena -0.5 * 2 copy: (120@120 extent: 30@30)) displayAt: 10@10 zoom: 15
	"
"
	(self asForm magnifyBy: anInteger) displayAt: aPoint.
"
	| max min k b g r |
	min _ self min.
	max _ self max.
	max > min ifFalse: [
		max _ min + 1 ].

	"Make positive values gray, and negative values green"
	k _ 255.0 / (max max: min negated).
	((self as32bitFormEvaluating: [ :index :word |
		g _ (((elements at: index) abs) * k + 0.5) truncated min: 255 max: 0.
		(elements at: index) > 0
			ifTrue: [ r _ b _ g ]
			ifFalse: [ r _ b _ 0 ].
		word at: 1 put: b; at: 2 put: g; at: 3 put: r ]
	)
			magnifyBy: anInteger) displayAt: aPoint.! !

!FloatImage methodsFor: 'displaying' stamp: 'jmv 5/4/2016 11:11'!
displayWithGammaAt: aPoint
	"Values are in linear space, and [0.0 1.0]. Clip values out of range.
	Assume receiver is in linear scale.
	Apply appropriate gamma encoding to make good use of pixel dynamic range.
	(See #linearSpaceToGammaEncoded and #linearSpaceToGammaEncoded)"

	self asGammaFormUnitRange displayAt: aPoint! !

!FloatImage methodsFor: 'dynamic range operations' stamp: 'jmv 6/4/2015 10:56'!
ditherForHistogramEq
	"Add some uniform random noise so image values are distributed over the entire real [0.0 .. 1.0] interval.
	This is good, for example, for histogram equalization, as it allows getting a really flat histogram after equalization,
	covering esentially all possible values.
	Assume 256 levels, i.e. an image built from an 8bpp Form or file.
	Tweak as needed, etc."

	| k random levelsInOriginalImage |
	levelsInOriginalImage _ 256.
	k _ 1.0 / (levelsInOriginalImage-1).
	random _ Random new.
	self replaceValues: [ :i :j :v | v + (random next - 0.5*k) min: 1 max: 0 ].! !

!FloatImage methodsFor: 'private' stamp: 'jmv 10/19/2017 13:17:52'!
erodedBy: erosionRadious
	"
	(FloatImage fromArrayOfArrays: #(
		#( 1 1 1 1 1 0)
		#( 1 0 1 1 1 1)
		#( 1 1 1 1 1 1)
		#( 1 1 1 1 1 1)
		#( 1 1 1 1 1 1)
		#( 1 1 1 1 1 1))) erodedBy: 1
	"
	| answer aux |
	aux _ FloatImage extent: self extent.
	answer _ nil.

	erosionRadious negated to: erosionRadious do: [ :dy |
		erosionRadious negated to: erosionRadious do: [ :dx |
			aux *= 0. aux += 1.
			aux fillWith: self displaced: dx@dy.
			answer ifNil: [
				answer _ aux copy ]
			ifNotNil: [
				answer *= aux ]]].
	^ answer! !

!FloatImage methodsFor: 'conversion' stamp: 'jmv 4/28/2015 11:13'!
fill32BitForm: a32BitForm
"Se podria pensar en meter min y max, y clamping, multiplicando por una cte, y usando un Bitmap de 2^24 entradas (16Mb!!) como colorMap, para mappear todas las combinaciones posibles que overfloween, a 255 en el form destino.
Esto serviria para, por ejemplo, ajustes de brillo, etc en una gui"

	| bb magic |
	a32BitForm makeAllPixelsOpaqueBlack.
	magic _ 128.0.
	bb _ BitBlt toForm: a32BitForm.
	bb combinationRule: 7. "or"
	bb sourceX: 0; sourceY: 0.
	bb destX: 0 destY: 0 width: width height: height.

	self += magic.
	bb sourceForm: (Form new hackBitsForFloatMatrix: self).
	bb colorMap: (ColorMap masks: { 0. 16rFF00. 16rFF00. 16rFF00 } shifts: { 0. 8. 0. -8 }).
	bb copyBits.
	self -= magic! !

!FloatImage methodsFor: 'conversion' stamp: 'jmv 4/28/2015 11:11'!
fill32BitForm: a32BitForm withG: greenMatrix b: blueMatrix 
"Se podria pensar en meter min y max, y clamping, multiplicando por una cte, y usando un Bitmap de 2^24 entradas (16Mb!!) como colorMap, para mappear todas las combinaciones posibles que overfloween, a 255 en el form destino.
Esto serviria para, por ejemplo, ajustes de brillo, etc en una gui"

	| bb magic |
	a32BitForm makeAllPixelsOpaqueBlack.
	magic _ 128.0.
	bb _ BitBlt toForm: a32BitForm.
	bb combinationRule: 7. "or"
	bb sourceX: 0; sourceY: 0.
	bb destX: 0 destY: 0 width: width height: height.

	self += magic.
	bb sourceForm: (Form new hackBitsForFloatMatrix: self).
	bb colorMap: (ColorMap masks: { 16rFF00. 0. 0. 0 } shifts: { 8. 0. 0. 0 }).
	bb copyBits.
	self -= magic.

	greenMatrix += magic.
	bb sourceForm: (Form new hackBitsForFloatMatrix: greenMatrix).
	bb colorMap: (ColorMap masks: { 16rFF00. 0. 0. 0 } shifts: { 0. 0. 0. 0 }).
	bb copyBits.
	greenMatrix -= magic.

	blueMatrix += magic.
	bb sourceForm: (Form new hackBitsForFloatMatrix: blueMatrix).
	bb colorMap: (ColorMap masks: { 16rFF00. 0. 0. 0 } shifts: { -8. 0. 0. 0 }).
	bb copyBits.
	blueMatrix -= magic.! !

!FloatImage methodsFor: 'private' stamp: 'jmv 5/22/2015 12:19'!
fillElementsWith: aFloatArray displacedBy: anInteger

	anInteger < 0
		ifTrue: [
			elements replaceFrom: 1 to: elements size + anInteger with: aFloatArray startingAt: 1 - anInteger.
			elements replaceFrom: elements size + anInteger+1 to: elements size with: aFloatArray startingAt: 1
		] ifFalse: [
			elements replaceFrom: 1+anInteger to: elements size with: aFloatArray startingAt: 1.
			elements replaceFrom: 1 to: 1 + anInteger - 1 with: aFloatArray startingAt: elements size - anInteger+1 ]! !

!FloatImage methodsFor: 'conversion' stamp: 'jmv 7/18/2016 11:41:39'!
fillGrayForm: aGrayForm
"Se podria pensar en meter min y max, y clamping, multiplicando por una cte, y usando un Bitmap de 2^24 entradas (16Mb!!) como colorMap, para mappear todas las combinaciones posibles que overfloween, a 255 en el form destino.
Esto serviria para, por ejemplo, ajustes de brillo, etc en una gui"

	| bb magic |
	aGrayForm bits atAllPut: 0.
	magic _ 128.0.
	bb _ BitBlt toForm: aGrayForm.
	bb combinationRule: 7. "or"
	bb sourceX: 0; sourceY: 0.
	bb destX: 0 destY: 0 width: width height: height.

	self += magic.
	bb sourceForm: (Form new hackBitsForFloatMatrix: self).
	bb colorMap: (ColorMap masks: { 0. 16rFF00. 16rFF00. 16rFF00 } shifts: { 0. 8. 0. -8 }).
	bb copyBits.
	self -= magic! !

!FloatImage methodsFor: 'operations' stamp: 'jmv 9/1/2014 14:27'!
fillRect: aRectangle with: aFloat
	aRectangle top to: aRectangle bottom-1 do: [ :y |
		aRectangle left to: aRectangle right-1 do: [ :x |
			self x: x y: y put: aFloat ]]! !

!FloatImage methodsFor: 'copying' stamp: 'jmv 5/22/2015 12:19'!
fillWith: aFloatImage cheaplyDisplaced: deltaPoint
	"Answer a new image our contents, sort of displaced by aPoint.
	Not really precise. Shift in the X direction wrap around the next or previous line!!
	Warning: Assumes 
		aFloatImage extent = self extent
		displacement is integer!!
	"
	
	self fillElementsWith: aFloatImage elements displacedBy: deltaPoint y*width + deltaPoint x! !

!FloatImage methodsFor: 'repair' stamp: 'jmv 8/3/2016 14:05:28'!
findUnusuallyDarkPixels
	"Answer a list of dead pixels, or pixels that are significantly darker than most in the same row."
	"These thresholds were found experimenting with sample images (So far, with just one image!!)
	They are meant to be refined as we download more real satellite images, especially if we get enough to take medians."

	| row median v dark dark2 darkAsSet |
	dark _ Array streamContents: [ :stream |
		1 to: height do: [ :i |
			row _ self i: i.
			median _ row median.
			1 to: width do: [ :j |
				v _ self i: i j: j.
				"See method comment"
				(median - v > (10/256.0) and: [v / median < 0.85]) ifTrue: [
					"debug aid"
					"{j@i-1. v. median rounded asFloat. (v / median) asFloat printStringFractionDigits: 2. (v - median) printStringFractionDigits: 2} print."
					stream nextPut: j@i ]]]].

	darkAsSet _ dark asSet.

	dark2 _ Set new.
	dark do: [ :each |	| i j |
		{-1@0. 1@0. 0@ -1. 0@ 1} do: [ :d |
			j _ each x + d x.
			i _ each y + d y.
			(darkAsSet includes: j@i) ifFalse: [
				(j@i between: 1@1 and: width@height) ifTrue: [
					row _ self i: i.
					median _ row median.
					v _ self i: i j: j.
					"See method comment"
					(median - v > (20/256.0) and: [v / median < 0.9]) ifTrue: [
					"debug aid"
						"{j@i-1. v. median rounded asFloat. (v / median) asFloat printStringFractionDigits: 2. (median - v) printStringFractionDigits: 2} print."
						dark2 add: j@i ]]]]].

	^(dark, dark2 asArray sort: [ :p1 :p2 | p1 y < p2 y or: [p1 y = p2 y and: [p1 x < p2 x]]]) print! !

!FloatImage methodsFor: 'filtering' stamp: 'jmv 12/13/2016 10:52:05'!
fourierDeconvolutionOf: blurredImages deconvolve: kernels maxNoiseAmplification: maxNoiseFactor
	"
	Experiments towards good deconvolution with SEVERAL images and kernels.
	The idea is (for example) that severa exposure times or other means to control blur might yield kernels that individually have frequencies with zero or near zero coefficients, but by taking the best rom each, a much better rebuild is possible.
	
	Could be brought closer to the newer #deconvolutionWith:maxNoiseAmplification:
	
	blurredImages are the results of the convolution of some unknown image with various kernels.
	Try to rebuild the original unknown image: Fill the receiver with the result of joint deconvolution.
	Note: Deconvolution is computed in the complex DFT space. Input is assumed to be real, and answer is real too.
	Note 2: This method does a Circular Deconvolution.
	
	Do Not perform the final inverse FFT, but answer it.
	Let callers do it if desired (or maybe, do stuff with coefficients in Fourier space)
	"
	"
	A vast improvement in efficiency could be to take small blurKernels, not requiring them to be of the same size as the images, and it could build just one larger 1-d array for the row/column fft."
	" maxNoiseFactor era 'infinito' hasta que me di cuenta que en algunos casos ayuda"
	| count imageReals imageImaginaries kernelReals kernelImaginaries resultI fft operandNormSquared weight weightSum amplificationFactor
	imageR imageI kernelR kernelI
	eSum fSum
	a b c d e f |
	count _ blurredImages size.
	imageReals _ Array new: count.
	imageImaginaries _ Array new: count.
	kernelReals _ Array new: count.
	kernelImaginaries _ Array new: count.
	fft _ FFT2D new.
	1 to: count do: [ :idx |
		imageR _ (blurredImages at: idx) copy.
		imageReals at: idx put: imageR.
		imageI _ FloatImage width: width height: height.
		imageImaginaries at: idx put: imageI.
		fft realData: imageR imagData: imageI.
		fft transformForward: true.
		kernelR _ FloatImage width: width height: height.
		kernelR fillWith: (kernels at: idx).
		kernelReals at: idx put: kernelR.
		kernelI _ FloatImage width: width height: height.
		kernelImaginaries at: idx put: kernelI.
		fft realData: kernelR imagData: kernelI.
		fft transformForward: true ].
	resultI _ FloatImage width: width height: height.
	1 to: height do: [ :y |
		1 to: width do: [ :x |
			eSum _ 0.0.
			fSum _ 0.0.
			weightSum _ 0.0.
			1 to: count do: [ :idx |
				c _ (kernelReals at: idx) x: x y: y.
				d _ (kernelImaginaries at: idx) x: x y: y.
				operandNormSquared _ c * c + (d * d).
				operandNormSquared isZero ifFalse: [
					a _ (imageReals at: idx) x: x y: y.
					b _ (imageImaginaries at: idx) x: x y: y.
					"operandNormSquared and weight kept just to illustrate what the heck are we doing here..."
					"Compute complex division: e+if = (a+ib) / (c+id)"
					e _ a * c + (b * d) / operandNormSquared.
					f _ b * c - (a * d) / operandNormSquared.
					weight _ operandNormSquared sqrt.
					eSum _ e * weight + eSum.
					fSum _ f * weight + fSum.
					weightSum _ weightSum + weight ]].
			amplificationFactor _ weightSum isZero
				ifTrue: [ 0.0 ]
				ifFalse: [ 1.0 / weightSum min: maxNoiseFactor ].
			eSum _ eSum * amplificationFactor.
			fSum _ fSum * amplificationFactor.
			self x: x y: y put: eSum.
			resultI x: x y: y put: fSum.
			]].
	fft realData: self imagData: resultI.
	"Do not transform back self"
	"fft transformForward: false"
	^fft! !

!FloatImage methodsFor: 'resampling' stamp: 'jmv 8/31/2015 12:45'!
fourierDoubledBilinearTransformedBy: anAffineTransformation resultExtent: resultExtentOrNil
	| biggerExtent biggerImaginary biggerReal fftExtent imaginary real 
	e resultWidth resultHeight scaled inverseTransformation p v totalScale |
	fftExtent _ 2 raisedToInteger: ((width max: height) log: 2) ceiling.
	real _ FloatImage extent: fftExtent@fftExtent.
	imaginary _ FloatImage extent: fftExtent@fftExtent.
	real fillWith: self.
	real fftImaginary:  imaginary forward: true.
	
	biggerExtent _ 2 raisedToInteger: ((width max: height) log: 2) ceiling + 1.
	totalScale _ biggerExtent / width.
	biggerReal _ FloatImage extent: biggerExtent@biggerExtent.
	biggerImaginary _ FloatImage extent: biggerExtent@biggerExtent.
	biggerReal fillLowFFTFrequenciesWith: real.
	biggerImaginary fillLowFFTFrequenciesWith: imaginary.
	biggerReal *= 4.
	biggerImaginary *= 4.
	biggerReal fftImaginary:  biggerImaginary forward: false.
	
	e _ resultExtentOrNil ifNil: [ self resultExtentFor: anAffineTransformation ].
	resultWidth _ e x.
	resultHeight _ e y.
	scaled _ self appropriateResultClass width: resultWidth height: resultHeight.
			inverseTransformation _ anAffineTransformation inverseTransformation.
	1 to: resultHeight do: [ :y |
		1to: resultWidth do: [ :x |
			p _ inverseTransformation transform: x@y.
			p _ p * totalScale.
			"cuando haya escala, no estoy seguro si es aca que hay que restar 1 para que no camine..."
			v _ biggerReal bilinearInterpolatedValueAtX: p x-1 y: p y-1.
			scaled x: x y: y put: v
		]].
	^scaled! !

!FloatImage methodsFor: 'dynamic range operations' stamp: 'jmv 8/23/2014 23:41'!
gammaEncodedToLinearSpace
	"Assume the receiver is in gamma encoded space (values are proportional luminance, framebuffer contents).
	Modify it so values are proportional to photon count (intensity of light, radiance flux).
	Useful, for example, for taking an image read from a jpg or png file (gamma encoded)
	and preparing it for processing in the linear space.
	"
	self replaceValues: [ :i :j :v | Color sRGBGammaToLinear: v ]! !

!FloatImage methodsFor: 'operations' stamp: 'jmv 8/5/2014 16:46'!
guardZone: aPoint
	"Guard Zones
	((FloatImage fromArrayOfArrays: #(
		(9 9 9 9 9 9)
		(9 9 9 9 9 9)
		(9 9 9 0 9 9)
		(9 9 9 0 9 9)
		(9 9 9 0 9 9)
		(9 9 9 9 9 9)
		))
			guardZone: 1@2;
			asFloatMatrix)
			print
	"
	self guardZone: aPoint topLeftNullFill: 0@0 bottomRightNullFill: 0@0! !

!FloatImage methodsFor: 'private' stamp: 'jmv 5/21/2015 11:02'!
guardZone: guardZone topLeftNullFill: leadingNulls bottomRightNullFill: trailingNulls
	"Guard Zones
	(((FloatImage fromArrayOfArrays: #(
		(9 9 9 9 9 9 9 9)
		(9 9 9 9 9 9 9 9)
		(9 9 9 9 9 9 9 9)
		(9 9 9 -9 -9 -9 9 9)
		(9 9 9 -9 -9 -9 -9 9)
		(9 9 9 -9 -9 -9 9 9)
		(9 9 9 -9 -9 -9 9 9)
		(9 9 9 9 9 9 9 9)
		))
			guardZone: 3@2 topLeftNullFill: 1@0 bottomRightNullFill: 0@1;
			print;
			asForm) magnifyTo: 80@80) display
	"
	| outerRect innerRect nullValue |
	self assert: width > (guardZone x * 2 + leadingNulls x + trailingNulls x).
	self assert: height > (guardZone y * 2 + leadingNulls y + trailingNulls y).
	nullValue _ elements sum / elements size.
	outerRect _ 1@1 extent: width@height.
	innerRect _ outerRect insetBy: (leadingNulls + guardZone abs ceiling corner: trailingNulls + guardZone abs ceiling).
	outerRect areasOutside: innerRect do: [ :rect |
		rect top to: rect bottom-1 do: [ :y |
			rect left to: rect right-1 do: [ :x |
				self guardZoneX: x y: y guardZone: guardZone innerRect: innerRect nullValue: nullValue ]]]! !

!FloatImage methodsFor: 'private' stamp: 'jmv 1/26/2015 15:33'!
guardZoneX: x y: y guardZone: guardZone innerRect: innerRect nullValue: nullValue
	"nullValue is the value to be used outside the guardZone, and the value the guardZone gradient goes to.
	It used to be zero, but it is better to be a parameter"
	| f xb yb keepLeft keepRight keepTop keepBottom |
	keepLeft _ innerRect left.
	keepTop _ innerRect top.
	keepRight _ innerRect right - 1.
	keepBottom _ innerRect bottom -1.
	guardZone x = 0 ifFalse: [
		x < keepLeft
			ifTrue: [
				xb _ keepLeft.
				yb _ xb-x / guardZone x * guardZone y + y.
				(yb >= keepTop and: [ yb <= keepBottom ]) ifTrue: [
					f _ x-xb / guardZone x abs+1 max: 0.0.
					^self x: x y: y put: (self bilinearInterpolatedValueAtX: xb y: yb) * f + (1-f * nullValue)]].
		x > keepRight
			ifTrue: [
				xb _ keepRight.
				yb _ xb-x / guardZone x * guardZone y + y.
				(yb >= keepTop and: [ yb <= keepBottom ]) ifTrue: [
					f _ xb-x / guardZone x abs+1 max: 0.0.
					^self x: x y: y put: (self bilinearInterpolatedValueAtX: xb y: yb) * f + (1-f * nullValue)]]].
	guardZone y = 0 ifFalse: [
		y < keepTop
			ifTrue: [
				yb _ keepTop.
				xb _ yb-y / guardZone y * guardZone x + x.
				(xb >= keepLeft and: [ xb <= keepRight ]) ifTrue: [
					f _ y - yb / guardZone y abs+1 max: 0.0.
					^self x: x y: y put: (self bilinearInterpolatedValueAtX: xb y: yb) * f + (1-f * nullValue)]].
		y > keepBottom
			ifTrue: [
				yb _ keepBottom.
				xb _ yb-y / guardZone y * guardZone x + x.
				(xb >= keepLeft and: [ xb <= keepRight ]) ifTrue: [
					f _ yb-y / guardZone y abs+1 max: 0.0.
					^self x: x y: y put: (self bilinearInterpolatedValueAtX: xb y: yb) * f + (1-f * nullValue)]]].
	self x: x y: y put: nullValue! !

!FloatImage methodsFor: 'repair' stamp: 'jmv 8/3/2016 14:09:01'!
highlight: correctionMap
	"Diagnostic, debugging, etc."

	| x x0 x1 y y0 y1 |
	1 to: correctionMap height do: [ :i |
		x _ correctionMap at: 1@i.
		y _ correctionMap at: 2@i.
		x0 _ correctionMap at: 3@i.
		x1 _ correctionMap at: 4@i.
		y0 _ correctionMap at: 5@i.
		y1 _ correctionMap at: 6@i.
		
		"Debug aid"
		self x: x y: y put: 0.
		self x: x0 y: y put: 130/256.0.
		self x: x1 y: y put: 130/256.0.
		self x: x y: y0 put: 130/256.0.
		self x: x y: y1 put: 130/256.0.
	].! !

!FloatImage methodsFor: 'statistics' stamp: 'jmv 8/9/2012 15:42'!
histogram
	^self histogram: 256! !

!FloatImage methodsFor: 'statistics' stamp: 'jmv 7/18/2016 23:13:49'!
histogram: slotCount
	| answer lowValue highValue halfSlot |
"	answer _ ImageHistogram3 new: slotCount."
	
	"Follow convention when converting Forms to FloatImage: we don't reach 1.0
	Alternatively, these could be min and max values in receiver..."
	lowValue _ 0.0.
	highValue _ 255/256.0.
	
	halfSlot _ highValue-lowValue / (slotCount-1) / 2.
	answer _ Histogram binCount: slotCount minValue: lowValue-halfSlot maxValue: highValue+halfSlot.
	elements do: [ :pixelValue |
		answer addValue: pixelValue ].
	^answer! !

!FloatImage methodsFor: 'dynamic range operations' stamp: 'jmv 6/4/2015 10:55'!
histogramEq
	"Could include random noise to distribute pixels over all possible values... Dithering...
	To do this properly we'd need to know more about distribution of values. For instance, if an 8-bit image, just use a 256 bins histogram and dither.
	But if the image is really Float, then do nothing!!

	FloatImage lena histogramEq; displayAt: 0@0.
	FloatImage lena ditherForHistogramEq; histogramEq; displayAt: 256@0.

	(FloatImage lena histogramEq; histogram) plot.
	(FloatImage lena ditherForHistogramEq; histogramEq; histogram) plot.
	"

	self applyMap: self histogram accumulatedAsMap! !

!FloatImage methodsFor: 'interpolation' stamp: 'jmv 1/27/2015 11:47'!
lanczosFilteredValueAtX: x y: y filterScale: filterScale
	"Apply a Lanczos resampling filter at x@y, answer the filtered value.
	Do not modify receiver in any way.
	
	This method is just a convenience call for #x:y:lanczosFilterScale:xFilterStore:yFilterStore:, useful if a single call is needed.
	"
	| n xFilter yFilter |

	"Build arrays for filters"
	n _ (4 / filterScale) floor +1.
	xFilter _ FloatArray new: n.
	yFilter _ FloatArray new: n.

	"And call real method"
	^self lanczosFilteredValueAtX: x y: y filterScale: filterScale xFilterStore: xFilter yFilterStore: yFilter! !

!FloatImage methodsFor: 'interpolation' stamp: 'jmv 5/4/2015 13:45'!
lanczosFilteredValueAtX: x y: y filterScale: filterScale xFilterStore: xFilter yFilterStore: yFilter
	"Apply a Lanczos resampling filter at x@y, answer the filtered value.
	Do not modify receiver in any way.
	
	xFilter and yFilter are filled here, but they are parameters so they can be reused for the many calls to resample an image.

	x and y can be any Float values in in [1.0 .. width] and [1.0 .. height].
	filterScale is the scale factor of the image resampling for which we build the Lanczos filter.
	For example, to build a result image of half the size of the original, use filterScale=0.5.
	It must be more than zero, and no more than 1. (0.0 .. 1.0].
	"
	| xStart xStop yStart yStop xn yn xx yy sum xFilterOffset yFilterOffset xIndex0 yIndex0 |

	"Validate inside some reasonable bounds. We allow extending up to 0.8 in any direction"
	xIndex0 _ x floor - 1.
	"x < 0.2 ifTrue: [ ^0.0 ]."			"Do it with integer arithmetic if at all possible"
	xIndex0 < -1 ifTrue: [ ^0.0 ].
	(xIndex0 = -1 and: [x < 0.2]) ifTrue: [ ^0.0 ].
	"x > (width + 0.8) ifTrue: [ ^0.0 ]."	"Do it with integer arithmetic if at all possible"
	xIndex0 > width ifTrue: [ ^0.0 ].
	(xIndex0 = width and: [x > (width + 0.8)]) ifTrue: [ ^0.0 ].
	yIndex0 _ y floor - 1.
	"y < 0.2 ifTrue: [ ^0.0 ]."			"Do it with integer arithmetic if at all possible"
	yIndex0 < -1 ifTrue: [ ^0.0 ].
	(yIndex0 = -1 and: [y < 0.2]) ifTrue: [ ^0.0 ].
	"y > (height + 0.8) ifTrue: [ ^0.0 ]."	"Do it with integer arithmetic if at all possible"
	yIndex0 > height ifTrue: [ ^0.0 ].
	(yIndex0 = height and: [y > (height + 0.8)]) ifTrue: [ ^0.0 ].

	"Image area used by the filter. If it goes outside image bounds, it will be corrected below."
	xStart _ (x - (2/filterScale)) ceiling.
	xStop _ (x + (2/filterScale)) floor.
	yStart _ (y - (2/filterScale)) ceiling.
	yStop _ (y + (2/filterScale)) floor.

	"Filter size"
	xn _ xStop - xStart + 1.
	yn _ yStop - yStart + 1.

	"Build filters"
	1 to: xn do: [ :i |
		xx _  (i+xStart-1-x) * filterScale.
		xFilter at: i put: (FloatImage lanczosFilter2: xx) ].
	"Filter normalization is needed to avoid a slight cuadricule like pattern when resizing"
	xFilter *= (1.0/xFilter sum).
	1 to: yn do: [ :i |
		yy _  (i+yStart-1-y) * filterScale.
		yFilter at: i put: (FloatImage lanczosFilter2: yy) ].
	"Same as for x filter."
	yFilter *= (1.0/yFilter sum).

	"If filters should have gone outside image bounds, act as if image values at bounds extended beyond them.
	For this, reduce iteration bounds, and accumulate filter coefficients that would be ignored at the correct filter end."
	xFilterOffset _ 1-xStart.
	xStart < 1 ifTrue: [
		sum _ 0.0.
		1 to: xFilterOffset+1 do: [ :xxx |
			sum _ (xFilter at: xxx) + sum ].
		xFilter at: xFilterOffset+1 put: sum.
		xStart _ 1 ].
	xStop > width ifTrue: [
		sum _ 0.0.
		width+xFilterOffset to: xn do: [ :xxx |
			sum _ (xFilter at: xxx) + sum ].
		xFilter at: width+xFilterOffset put: sum.
		xStop _ width ].
	yFilterOffset _ 1-yStart.
	yStart < 1 ifTrue: [
		sum _ 0.0.
		1 to: yFilterOffset+1 do: [ :yyy |
			sum _ (yFilter at: yyy) + sum ].
		yFilter at: yFilterOffset+1 put: sum.
		yStart _ 1 ].
	yStop > height ifTrue: [
		sum _ 0.0.
		height+yFilterOffset to: yn do: [ :yyy |
			sum _ (yFilter at: yyy) + sum ].
		yFilter at: height+yFilterOffset put: sum.
		yStop _ height ].

	"Apply filter."
	sum _ 0.0.
	yStart to: yStop do: [ :yyy |
		xStart to: xStop do: [ :xxx |
			sum _ (self x: xxx y: yyy) * (xFilter at: xxx+xFilterOffset) * (yFilter at: yyy+yFilterOffset) + sum ]].
	^sum! !

!FloatImage methodsFor: 'interpolation' stamp: 'jmv 1/27/2015 11:47'!
lanczosFilteredValueOLDAtX: x y: y filterScale: filterScale xFilterStore: xFilter yFilterStore: yFilter
	"Apply a Lanczos resampling filter at x@y, answer the filtered value.
	Do not modify receiver in any way.
	
	xFilter and yFilter are filled here, but they are parameters so they can be reused for the many calls to resample an image.

	x and y can be any Float values in in [1.0 .. width] and [1.0 .. height].
	filterScale is the scale factor of the image resampling for which we build the Lanczos filter.
	For example, to build a result image of half the size of the original, use filterScale=0.5.
	It must be more than zero, and no more than 1. (0.0 .. 1.0]."
	| xStart xStop yStart yStop xn yn xx yy sum |

	"Image area used by the filter"
	xStart _ (x - (2/filterScale)) ceiling.
	xStop _ (x + (2/filterScale)) floor.
	yStart _ (y - (2/filterScale)) ceiling.
	yStop _ (y + (2/filterScale)) floor.

	"Filter size"
	xn _ xStop - xStart + 1.
	yn _ yStop - yStart + 1.

	"Build filters"
	1 to: xn do: [ :i |
		xx _  (i+xStart-1-x) * filterScale.
		xFilter at: i put: (FloatImage lanczosFilter2: xx) * filterScale ].
	1 to: yn do: [ :i |
		yy _  (i+yStart-1-y) * filterScale.
		yFilter at: i put: (FloatImage lanczosFilter2: yy) * filterScale].

	"Apply filter. Act as if image values at bounds extended beyond them."
	sum _ 0.0.
	yStart to: yStop do: [ :yyy |
		xStart to: xStop do: [ :xxx |
			sum _ (self x: (xxx min: width max: 1) y: (yyy min: height max: 1)) * (xFilter at: xxx-xStart+1) * (yFilter at: yyy-yStart+1) + sum ]].
	
	^sum! !

!FloatImage methodsFor: 'resampling' stamp: 'jmv 5/22/2015 12:27'!
lanczosTransformedBy: anAffineTransformation resultExtent: resultExtentOrNil
	"Answer a new image.
	anAffineTransformation specifies the transformation from us to the result image.
	resultExtentOrNil ifNil: [ make it large enough ]

	Warning: there could be black areas in the result, and/or some image content in self could be lost.
	This depends on the arguments.
	
	Note about OpenCV's INTER_LANCZOS4:
		According to online documentation, It looks like OpenCV a Lanczos filter of fixed support. 
		That means that the filter is fitted to the sampling frequency of the source image.
		So, no real filtering is done, just interpolation. Aliasing is expected when zooming out!!
		http://docs.opencv.org/modules/imgproc/doc/geometric_transformations.html#resize

	This method of image resampling is very good for scale < 0.9, because of good filtering.
	Even in these cases, Area filtering might be preferred, because it is quite good and much faster.

	For scale close to 1.0 and greater, no filtering is needed. In such cases a good interpolation
	like #bSplineTransformedBy:resultExtent: could give better results, and better performance
	(especially if many resamplings will be done on the same source image).
	"
	| scaled resultHeight resultWidth p filterScale n xFilter yFilter v inverseTransformation e |
	e _ resultExtentOrNil ifNil: [ self resultExtentFor: anAffineTransformation ].
	resultWidth _ e x.
	resultHeight _ e y.
	scaled _ self appropriateResultClass width: resultWidth height: resultHeight.
	inverseTransformation _ anAffineTransformation inverseTransformation.

	"If downscaling (zooming out, filterScale<1, subsampling) remove frequencies above sf, as they would become aliasing in the result.
	If upscaling (zooming in, filterScale > 1), avoid creating frequencies above 1 in the result, as they were not in ourselves."
	filterScale _ ((resultWidth / width) min: (resultHeight / height)) min: 1.0.
	n _ (4 / filterScale) floor +1.
	xFilter _ FloatArray new: n.
	yFilter _ FloatArray new: n.
	1 to: resultHeight do: [ :y |
		1 to: resultWidth do: [ :x |
			p _ inverseTransformation transform: x@y.
			v _ self lanczosFilteredValueAtX: p x y: p y filterScale: filterScale xFilterStore: xFilter yFilterStore: yFilter.
			scaled x: x y: y put: v ]].
	^scaled! !

!FloatImage methodsFor: 'filtering' stamp: 'jmv 6/28/2017 15:38:48'!
laplacian
	"A Sobel like operator for edge detection.
	See https://en.wikipedia.org/wiki/Sobel_operator
	The coding style can be improved (but do it without degrading performance!!)"
	
	| aux yFiltered |
	yFiltered _ self * -4.
	aux _ FloatImage extent: self extent.
	
	aux fillWith: self displaced: 0 @ -1.
	yFiltered elements += aux elements.
	aux fillWith: self displaced: 0 @ 1.
	yFiltered elements += aux elements.
	aux fillWith: self displaced: -1 @ 0.
	yFiltered elements += aux elements.
	aux fillWith: self displaced: 1 @ 0.
	yFiltered elements += aux elements.
	^yFiltered! !

!FloatImage methodsFor: 'dynamic range operations' stamp: 'jmv 8/23/2014 23:41'!
linearSpaceToGammaEncoded
	"Assume the receiver is in linear space (values are proportional to photon count).
	Modify it so values are proportional to human perceived brightness.
	Useful, for example, for displaying:
		aFloatImageInLinearSpace copy linearSpaceToGammaEncoded displayAt: 0@0
	"
	self replaceValues: [ :i :j :v | Color linearTosRGBGamma: v ]! !

!FloatImage methodsFor: 'statistics' stamp: 'jmv 9/25/2012 15:33'!
mean: aRect
	"
	| i |
	i _ FloatImage width: 1 height: 1.
	i += 1.
	(i mean: (1@1 extent: 1@1)) print
	"
	| count mean |
	count _ aRect area.
	mean _ 0.0.
	aRect top to: aRect bottom-1 do: [ :y |
		aRect left to: aRect right-1 do: [ :x |
			mean _ mean + (self x: x y: y) ]].
	^mean / count! !

!FloatImage methodsFor: 'statistics' stamp: 'jmv 7/19/2016 12:04:46'!
median
	"Very expensive on large collections. Consider using a Histogram:
		aMatrix histogram median
	"
	^elements median! !

!FloatImage methodsFor: 'statistics' stamp: 'jmv 7/19/2016 12:21:17'!
mode
	"Very expensive on large collections. Consider using a Histogram:
		aMatrix histogram mode
	"
	^elements mode! !

!FloatImage methodsFor: 'resampling' stamp: 'jmv 5/22/2015 12:27'!
nearestNeighborTransformedBy: anAffineTransformation resultExtent: resultExtentOrNil
	"Answer a new image.
	anAffineTransformation specifies the transformation from us to the result image.
	resultExtentOrNil ifNil: [ make it large enough ]

	Warning: there could be black areas in the result, and/or some image content in self could be lost.
	This depends on the arguments.

	Evaluate a Nearest Neighbor (i.e. very cheap, low quality) interpolation.
	Like OpenCV's CV_INTER_NN
	"
	| scaled resultHeight resultWidth p v inverseTransformation e |
	e _ resultExtentOrNil ifNil: [ self resultExtentFor: anAffineTransformation ].
	resultWidth _ e x.
	resultHeight _ e y.
	scaled _ self appropriateResultClass width: resultWidth height: resultHeight.
	inverseTransformation _ anAffineTransformation inverseTransformation.
	1 to: resultHeight do: [ :y |
		1to: resultWidth do: [ :x |
			p _ inverseTransformation transform: x@y.
			"Protect against invalid access"
			(p >= (0.5@0.5) and: [ p < (width@height + 0.5) ]) ifTrue: [
				v _ self x: p x rounded y: p y rounded.
				scaled x: x y: y put: v 
			]
		]].
	^scaled! !

!FloatImage methodsFor: 'dynamic range operations' stamp: 'jmv 8/23/2014 23:46'!
normalizeValues
	"Make min pixel value 0.0, and max pixel value 1.0.
	Adjust pixel values proportionally"

	elements -= elements min.
	elements *= (1.0 / elements max)! !

!FloatImage methodsFor: 'dynamic range operations' stamp: 'jmv 8/23/2014 23:41'!
normalizeValuesFloor: min ceiling: max
	"Make argument min pixel value 0.0, and argument max pixel value 1.0.
	Adjust pixel values proportionally. Clamp values outside."

	self replaceValues: [ :i :j :v | v - min / max min: 1.0 max: 0.0 ]! !

!FloatImage methodsFor: 'normalized coordinates' stamp: 'jmv 2/25/2016 16:51'!
normalizedCoordinatesForOneBased: oneBasedCoordinates
	"
	oneBasedCoordinates in [1..width] x [1..height] (might be float or integer)
	answer in [0.0 .. 1.0] x [0.0 .. 1.0]

	In normalized coordinates:
		0.0 means first sample (x=1)
		1.0 means last sample (x=width)

	(FloatImage extent: 11@11) normalizedCoordinatesForOneBased: 1@11
	(FloatImage extent: 11@11) normalizedCoordinatesForOneBased: 2@10
	"

	^((oneBasedCoordinates x-1.0)/(width-1)) @ ((oneBasedCoordinates y-1.0)/(height-1))! !

!FloatImage methodsFor: 'normalized coordinates' stamp: 'jmv 2/25/2016 16:52'!
oneBasedCoordinatesForNormalized: normalizedCoordinates
	"
	normalizedCoordinates in [0.0 .. 1.0] x [0.0 .. 1.0]
	answer in [1..width] x [1..height]
	Answer is Float!!

	In normalized coordinates:
		0.0 means first sample (x=1)
		1.0 means last sample (x=width)

	(FloatImage extent: 11@11) oneBasedCoordinatesForNormalized: 0.0@1.0
	(FloatImage extent: 11@11) oneBasedCoordinatesForNormalized: 0.1@0.9
	"
"
	| i oneBased |
	i _ FloatImage extent: 4@4.
	1 to: i width do: [ :x |
		1 to: i height do: [ :y |
			i x: x y: y put: (x-1 * 255.0 / (i width-1)) ]].
	i print.
	'' print.
	0.0 to: 1.0 count: 300 do: [ :x | 
		oneBased _ i oneBasedCoordinatesForNormalized: x@0.5.
		(i bilinearInterpolatedClampToEdgeValueAtX: oneBased x y: oneBased y) print ]. nil
"

	^ (normalizedCoordinates x*(width-1)+1) @ (normalizedCoordinates y*(height-1)+1)! !

!FloatImage methodsFor: 'normalized coordinates' stamp: 'jmv 2/26/2016 11:32'!
oneBasedCoordinatesForOpenCLNormalized: normalizedCoordinates
	"
	normalizedCoordinates in [0.0 .. 1.0] x [0.0 .. 1.0], with OpenCL semantics (See below)
	answer in [1..width] x [1..height]
	Answer is Float!!

	In OpenCL style normalized coordinates:
		0.0 means half pixel to the left of the first sample (x=0.5)
		1.0 means half pixel to the right of the last sample (x=width+0.5)

	(FloatImage extent: 10@10) oneBasedCoordinatesForOpenCLNormalized: 0.05@0.95
	(FloatImage extent: 10@10) oneBasedCoordinatesForOpenCLNormalized: 0.0@1.0
	(FloatImage extent: 10@10) oneBasedCoordinatesForOpenCLNormalized: 0.15@0.85

	This is what OpenCL does when doing linear interpolation with normalized coordinates
	See NormalizedCoordinatesExperiment in the OpenCL package"
"
	| i oneBased |
	i _ FloatImage extent: 4@4.
	1 to: i width do: [ :x |
		1 to: i height do: [ :y |
		i x: x y: y put: (x-1+0.5 * 255.0 / i width) ]].
	i print.
	'' print.
	0.0 to: 1.0 count: 300 do: [ :x | 
		oneBased _ i oneBasedCoordinatesForOpenCLNormalized: x@0.5.
		(i bilinearInterpolatedClampToEdgeValueAtX: oneBased x y: oneBased y) print ]. nil
"

	| x y |
	x _ normalizedCoordinates x*width+0.5.
	y _ normalizedCoordinates y*height+0.5.
	^ (x min: width max: 1) @ (y min: height max: 1)

	"
	| orig small rebuilt p v oneBased |
	orig _ FloatImage gradient: 256@256.
	orig display.

	small _ orig subsampleForOpenCLNormalizedCoordinates: 4@4.
	small _ FloatImage extent: 4@4.
	1 to: small height do: [ :y |
		1 to: small width do: [ :x |
			p _ orig oneBasedCoordinatesForOpenCLNormalized: (small openCLNormalizedCoordinatesForOneBased: x@y).
			v _ orig bilinearInterpolatedValueAtX: p x y: p y.
			small x: x y: y put: v ]].
	small display.

	rebuilt _ FloatImage extent: orig extent.
	1 to: rebuilt height do: [ :y |
		1 to: rebuilt width do: [ :x |
			oneBased _ small oneBasedCoordinatesForOpenCLNormalized: (x-1/(rebuilt width-1)) @ (y-1/(rebuilt height-1)).
			v _ small bilinearInterpolatedClampToEdgeValueAtX: oneBased x y: oneBased y.
			rebuilt x: x y: y put: v ]].
	rebuilt display.

	(rebuilt - orig + 0.5) histogram plot.
	(rebuilt - orig * 40 + 0.5) display
	"! !

!FloatImage methodsFor: 'normalized coordinates' stamp: 'jmv 2/26/2016 11:34'!
openCLNormalizedCoordinatesForOneBased: oneBasedCoordinates
	"
	oneBasedCoordinates in [1..width] x [1..height] (might be float or integer)
	answer in [0.0 .. 1.0] x [0.0 .. 1.0], with OpenCL semantics (See below)

	In OpenCL style normalized coordinates:
		0.0 means half pixel to the left of the first sample (x=0.5)
		1.0 means half pixel to the right of the last sample (x=width+0.5)

	(FloatImage extent: 10@10) openCLNormalizedCoordinatesForOneBased: 1@10
	(FloatImage extent: 10@10) openCLNormalizedCoordinatesForOneBased: 2@9

	This is what OpenCL does when doing linear interpolation with normalized coordinates
	See NormalizedCoordinatesExperiment in the OpenCL package"

	^ ((oneBasedCoordinates x-0.5)/width) @ ((oneBasedCoordinates y-0.5)/height)! !

!FloatImage methodsFor: 'filtering' stamp: 'jmv 7/31/2017 22:50:43'!
otfForPsf: trimmedPsf heightAndWidth: aNumber
	"Compute the OTF
	https://en.wikipedia.org/wiki/Optical_transfer_function
	"
	| movedToOriginPsf otfRealPart otfImaginaryPart fft |
	movedToOriginPsf _ trimmedPsf moveOriginToCenter: false.
	movedToOriginPsf normalizeSum.

	"Prepare convolution kernel for the actual extent of the tile."
	otfRealPart _ FloatImage width: aNumber height: aNumber.
	otfImaginaryPart _ FloatImage width: aNumber height: aNumber.
	otfRealPart fillLowFFTFrequenciesWith: movedToOriginPsf.

	fft _ FFT2D new.
	fft realData: otfRealPart imagData: otfImaginaryPart.
	fft transformForward: true.
	^ fft! !

!FloatImage methodsFor: 'statistics' stamp: 'jmv 7/19/2016 12:04:51'!
percentile: percentile
	"Very expensive on large collections. Consider using a Histogram:
		aMatrix histogram percentile: percentile
	"
	^elements percentile: percentile! !

!FloatImage methodsFor: 'resampling' stamp: 'jmv 6/15/2015 16:30'!
perspectiveTransformTopLeft: topLeft topRight: topRight bottomLeft: bottomLeft bottomRight: bottomRight
	"Resample the receiver to a new Float image.
	The arguments specify which point in the receiver maps to each corner of the result.
	The arguments might be Integer or Float points.
	Apply a PerspectiveTransformation"
"
| i p r |
i _ FloatImage lena.
p _ i perspectiveTransformTopLeft: -800@1 topRight: 256+800@1bottomLeft: 1@256 bottomRight: 256@256.
r _ p perspectiveTransformTopLeft: 111.1+0@1 topRight: 146+0.1@1bottomLeft: 1@256 bottomRight: 256@256.
i displayAt: 0@0.
p displayAt: 256@0.
r displayAt: 512@0.
"
	| scaled resultHeight resultWidth p v cornersInSource transformation resultCorners interpolator |
	cornersInSource _ {topLeft. topRight. bottomLeft. bottomRight }.
	resultWidth _ width.
	resultHeight _ height.
	
	scaled _ self appropriateResultClass width: resultWidth height: resultHeight.
	resultCorners _ { 1@1. resultWidth@1. 1@resultHeight. resultWidth@resultHeight }.
	
	transformation _ PerspectiveTransformation map: resultCorners to: cornersInSource.	
	interpolator _ BSplineInterpolatingImage from: self.

	1 to: resultHeight do: [ :y |
		1 to: resultWidth do: [ :x |
			p _ transformation map: x@y.
			v _ interpolator interpolatedValueAtX: p x y: p y.
			scaled x: x y: y put: v ]].
	^scaled! !

!FloatImage methodsFor: 'iterating' stamp: 'jmv 8/24/2014 00:12'!
pixelsAtChebyshevDistance: distance ofX: baseX y: baseY do: aBlock
	"Iterate those pixels that are, based on (x@y), at the required Chebysev distance.
	See http://en.wikipedia.org/wiki/Chebyshev_distance
	aBlock has 3 arguments: :x :y :i , where i is the index to use to access values and the other array ivars
	"
	"
	Testing code. The example is the one from wikipedia. Should give answer shown below.
	| image |
	image _ FloatImage width: 8 height: 8.
	1 to: 12 do: [ :d |
		image
			pixelsAtChebyshevDistance: d ofX: 6 y: 3 do: [ :x :y : i |
				(image x: x y: y) = 0 ifFalse: [ self error: 'two writes at same position!!!!!!!!!!' ].
				image x: x y: y put: d ]].
	image asFloatMatrix print

| 5.000 4.000 3.000 2.000 2.000 2.000 2.000 2.000 |
| 5.000 4.000 3.000 2.000 1.000 1.000 1.000 2.000 |
| 5.000 4.000 3.000 2.000 1.000 0.000 1.000 2.000 |
| 5.000 4.000 3.000 2.000 1.000 1.000 1.000 2.000 |
| 5.000 4.000 3.000 2.000 2.000 2.000 2.000 2.000 |
| 5.000 4.000 3.000 3.000 3.000 3.000 3.000 3.000 |
| 5.000 4.000 4.000 4.000 4.000 4.000 4.000 4.000 |
| 5.000 5.000 5.000 5.000 5.000 5.000 5.000 5.000 |
	"
	0 to: 3 do: [ :quadrant |
		self pixelsAtChebyshevDistance: distance ofX: baseX y: baseY inQuadrant: quadrant do: aBlock ]! !

!FloatImage methodsFor: 'iterating' stamp: 'jmv 8/24/2014 00:12'!
pixelsAtChebyshevDistance: distance ofX: baseX y: baseY inQuadrant: quadrantIndex do: aBlock
	"Iterate those pixels that are, based on (x@y), at the required Chebysev distance and in the required quadrant.
	See http://en.wikipedia.org/wiki/Chebyshev_distance
	quadrantIndex is:
		0: NE or NorthEast. UpOrEq and rightOrEq of (x@y). This is { (x1@y1) / x1 >= x and y1 <= y }
		1: NW or NorthWest. UpOrEq and left of (x@y). This is { (x1@y1) / x1 < x and y1 <= y }
		2: SW or SouthWest. Down and left of (x@y). This is { (x1@y1) / x1 < x and y1 > y }
		3: SE or ShouthEast. Up and rightOrEq of (x@y). This is { (x1@y1) / x1 >= x and y1 > y }
	So, NE includes row y and column x. NW does include row y but not column x.
	SW includes neither row y nor column x. SE includes column x, but not row y.
	aBlock has 3 arguments: :x :y :i , where i is the index to use to access values and the other array ivars
	"
	"
	Testing code. The example is the one from wikipedia. Should give answer shown below.
	| image |
	image _ IrregularSampledImage width: 8 height: 8.
	1 to: 12 do: [ :d |
		0 to: 3 do: [ :q |
			image
				pixelsAtChebyshevDistance: d
				ofX: 6 y: 3 inQuadrant: q
				do: [ :x :y : i |
					(image x: x y: y) = 0 ifFalse: [ self error: 'two writes at same position!!!!!!!!!!' ].
					image x: x y: y put: d ]]].
	image asFloatMatrix print

| 5.000 4.000 3.000 2.000 2.000 2.000 2.000 2.000 |
| 5.000 4.000 3.000 2.000 1.000 1.000 1.000 2.000 |
| 5.000 4.000 3.000 2.000 1.000 0.000 1.000 2.000 |
| 5.000 4.000 3.000 2.000 1.000 1.000 1.000 2.000 |
| 5.000 4.000 3.000 2.000 2.000 2.000 2.000 2.000 |
| 5.000 4.000 3.000 3.000 3.000 3.000 3.000 3.000 |
| 5.000 4.000 4.000 4.000 4.000 4.000 4.000 4.000 |
| 5.000 5.000 5.000 5.000 5.000 5.000 5.000 5.000 |
	"
	| byX byY firstX firstY lastX lastY i |
	(quadrantIndex = 0 or: [ quadrantIndex = 1 ]) ifTrue: [
		firstY _ baseY.
		byY _ -1 ]
	ifFalse: [
		firstY _ baseY + 1.
		byY _ 1 ].
	(quadrantIndex = 0 or: [ quadrantIndex = 3 ]) ifTrue: [
		firstX _ baseX.
		byX _ 1 ]
	ifFalse: [
		firstX _ baseX-1.
		byX _ -1 ].
	lastX _ distance * byX + baseX.
	lastY _ distance * byY + baseY.
	(lastY between: 1 and: height) ifTrue: [
		firstX to: (lastX min: width max: 1) by: byX do: [ :x |
			i _ lastY-1 * width + x.
			aBlock value: x value: lastY value: i ]].
	(lastX between: 1 and: width) ifTrue: [
		(lastY-byY min: height max: 1) to: firstY by: byY negated do: [ :y |
			i _ y-1 * width + lastX.
			aBlock value: lastX value: y value: i ]]! !

!FloatImage methodsFor: 'iterating' stamp: 'jmv 8/24/2014 00:12'!
pixelsCloserThanOrAtChebyshevDistance: distance ofX: baseX y: baseY do: aBlock
	"Iterate those pixels that are, based on (x@y), at the required Chebysev distance.
	See http://en.wikipedia.org/wiki/Chebyshev_distance
	aBlock has 3 arguments: :x :y :i , where i is the index to use to access values and the other array ivars
	"
	"
	Testing code. The example is the one from wikipedia. Should give answer shown below.
	| image |
	image _ FloatImage width: 8 height: 8.
	{4. 2} do: [ :d |
		image
			pixelsCloserThanChebyshevDistance: d ofX: 6 y: 3 do: [ :x :y : i |
				image x: x y: y put: d ]].
	image asFloatMatrix print

| 0.000 4.000 4.000 2.000 2.000 2.000 2.000 2.000 |
| 0.000 4.000 4.000 2.000 2.000 2.000 2.000 2.000 |
| 0.000 4.000 4.000 2.000 2.000 2.000 2.000 2.000 |
| 0.000 4.000 4.000 2.000 2.000 2.000 2.000 2.000 |
| 0.000 4.000 4.000 2.000 2.000 2.000 2.000 2.000 |
| 0.000 4.000 4.000 4.000 4.000 4.000 4.000 4.000 |
| 0.000 4.000 4.000 4.000 4.000 4.000 4.000 4.000 |
| 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 |
	"

	| i |
	(baseY - distance max: 1) to: (baseY + distance min: height) do: [ :y |
		(baseX - distance max: 1) to: (baseX + distance min: width) do: [ :x|
			i _ y-1 * width + x.
			aBlock value: x value: y value: i ]]! !

!FloatImage methodsFor: 'quantization' stamp: 'jmv 6/9/2015 15:22'!
quantizeTo: bits
	"Simulate quantization to the specified number of bits.
	Modify the receiver.
	Use no more than 2^bits different values.
	Assume input values in (0.0 .. 1.0)
	Truncate values. Result is less than 1.0.
	Clamp values outside (0.0 .. 1.0)
	
	Multiplying by 2^bits  gives all integer values, in the range [0 .. 2^bits-1].
	Useful for doing #asForm or #asFormG:b:

	| i bits |
	bits _ 3.
	i _ FloatImage fromArrayOfArrays: { (0.0 to: 1.0 count: 20) asFloatArray }.
	(i quantizeTo: bits ) * (2 raisedTo: bits)

	| i |
	i _ FloatImage fromArrayOfArrays: { (0.0 to: 1.0 count: 100000) asFloatArray }.
	(i quantizeTo: 3 ) elements asBag instVarNamed: 'contents'
	"
	| n q k |
	n _ 2 raisedTo: bits.
	k _ (n-1.0)/n.
	q _ 1.0/ n.
	self replaceValues: [ :i :j :v | (v truncateTo: q) min: k max: 0.0 ]! !

!FloatImage methodsFor: 'repair' stamp: 'jmv 8/3/2016 14:12:02'!
repair: correctionMap

	| x x0 x1 y y0 y1 v1 v2 |
	1 to: correctionMap height do: [ :i |
		x _ correctionMap at: 1@i.
		y _ correctionMap at: 2@i.
		x0 _ correctionMap at: 3@i.
		x1 _ correctionMap at: 4@i.
		y0 _ correctionMap at: 5@i.
		y1 _ correctionMap at: 6@i.

		v1 _ x1 = x0
			ifTrue: [self at: x0@y]
			ifFalse: [(self at: x0@y) interpolateTo: (self at: x1@y) at: (x-x0/(x1-x0))].
		v2 _ y1 = y0
			ifTrue: [self at: x@y0]
			ifFalse: [(self at: x@y0) interpolateTo: (self at: x@y1) at: (y-y0/(y1-y0))].
		self x: x y: y put: v1+v2*0.5
		].

"
roi _ 'C:\Users\Juan-Tuerca\BajadasFrescoYBatata\FM1-Fresco\image.160315010222._2240_0___640_5120_.png' asFileEntry formContents.
roi _ FloatImage fromGrayForm: roi.

w _ roi width.
h _ roi height.

darkPixels _ roi findUnusuallyDarkPixels.
map _ roi badPixelCorrectionMapFor: darkPixels.

showDarkPixels _ roi copy.
showDarkPixels highlight: map.
showDarkPixels writePNGfileNamed: 'showDarkPixels.png'.nil.

fixed _ roi copy.
fixed repair: map.
fixed writePNGfileNamed: 'fixed.png'.nil.
"! !

!FloatImage methodsFor: 'analysis' stamp: 'jmv 8/24/2017 11:27:31'!
repairLocalRelativeThreshold: fractionOfPixelValue
	"
	Evaluate aboveBlock for each pixel that is at least aNumber greater than 8 neighbours.
	Evaluate belowBlock for each pixel that is at least aNumber negated lower than 8 neighbours.
	"
	"
	| image |
	image := FloatImage lena.
	image i: 10 j: 10 put: 1.0.
	image i: 20 j: 20 put: 0.0.
	image displayAt: 0@0.
	image repairLocalRelativeThreshold: 0.2.
	image displayAt: 256@0
	"
	| diffElements displaced maxDiff maxDiffElements minDiff minDiffElements mean meanElements absoluteThreshold |
	minDiff _ (FloatImage extent: self extent) + 1.
	maxDiff _ (FloatImage extent: self extent) - 1.
	mean _ FloatImage extent: self extent.
	minDiffElements _ minDiff elements.
	maxDiffElements _ maxDiff elements.
	meanElements _ mean elements.

	"8 neighbors might be overkill, but better be safe."
	{ -1@0. -1@-1. 0@ -1. 1@ -1. 1@0. 1@1. 0@1. -1@1 } do: [ :delta |
		displaced _ self cheapDisplacedBy: delta.
		mean += displaced.
		diffElements _ (self - displaced) elements.
		1 to: minDiffElements size do: [ :idx |
			minDiffElements at: idx put: ( (minDiffElements at: idx) min: (diffElements at: idx)).
			maxDiffElements at: idx put: ( (maxDiffElements at: idx) max: (diffElements at: idx)) ]].

	1 to: elements size do: [ :idx |
		absoluteThreshold _ (elements at: idx) * fractionOfPixelValue.
		(minDiffElements at: idx) > absoluteThreshold ifTrue: [ elements at: idx put: (meanElements at: idx)/8 ].
		(maxDiffElements at: idx) < absoluteThreshold negated ifTrue: [ elements at: idx put: (meanElements at: idx)/8 ] ]! !

!FloatImage methodsFor: 'resampling-aux' stamp: 'jmv 1/27/2015 14:26'!
resultExtentFor: anAffineTransformation
	"Compute an appropriate extent for the result of applying anAffineTransformation to self.
	Guarantees that the image bottomRight is enough.
	Does not prevent clipping if result would require x or y with values less than 1!!"

	^(anAffineTransformation displayBoundsOfTransformOf: (0@0 extent: width@ height))
		corner! !

!FloatImage methodsFor: 'quantization' stamp: 'jmv 8/23/2014 23:41'!
round

	self replaceValues: [ :i :j :v | v rounded ]! !

!FloatImage methodsFor: 'resampling' stamp: 'jmv 4/8/2015 15:28'!
scaledBy: scale
	"Answer a new image. Extent is multiplied by scale (i.e. scale=0.5 means half size, scale=2 means twice size.
	Gives very good results for any scale values, including very large and very small.
	Uses unfiltered BSpline interpolation for large scale factors, and Lanczos filtering and resampling for small scale factors. 
	But you can use the methods specific to each technique if you know what you want.

	| original |
	original _ FloatImage checkerboard: 100@100 n: 50.
	(original scaledBy: 4.0) display.
	original display.
	(original scaledBy: 0.25) display.

	| original |
	original _ FloatImage from8BitColorForm: Cursor read as8BitColorForm.
	(original scaledBy: 4.0) display.
	original asForm displayAt: 16@0.
	(original scaledBy: 0.7) display.
	"
	| t |
	t _ AffineTransformation forImageZoom: scale.
	^scale < 0.8
		ifTrue: [
			scale < 0.25
				ifTrue: [self areaTransformedBy: t resultExtent: nil]
				ifFalse: [self lanczosTransformedBy: t resultExtent: nil]]
		ifFalse: [self bSplineTransformedBy: t resultExtent: nil ]! !

!FloatImage methodsFor: 'filtering' stamp: 'jmv 10/19/2017 12:43:43'!
scharr
	"A Sobel like operator for edge detection.
	See https://en.wikipedia.org/wiki/Sobel_operator
	The coding style can be improved (but do it without degrading performance!!)"
	
	| aux e xFiltered yFiltered |
	aux _ FloatImage extent: self extent.
	yFiltered _ FloatImage extent: self extent.
	xFiltered _ FloatImage extent: self extent.
	
	aux fillWith: self displaced: -1 @ -1.
	aux *= 3.
	yFiltered += aux.

	aux fillWith: self displaced: 0 @ -1.
	aux *= 10.
	yFiltered += aux.

	aux fillWith: self displaced: 1 @ -1.
	aux *= 3.
	yFiltered += aux.

	aux fillWith: self displaced: -1 @ 1.
	aux *= -3.
	yFiltered += aux.

	aux fillWith: self displaced: 0 @ 1.
	aux *= -10.
	yFiltered += aux.

	aux fillWith: self displaced: 1 @ 1.
	aux *= -3.
	yFiltered += aux.


	aux fillWith: self displaced: -1 @ -1.
	aux *= 3.
	xFiltered += aux.

	aux fillWith: self displaced: -1 @ 0.
	aux *= 10.
	xFiltered += aux.

	aux fillWith: self displaced: -1 @ 1.
	aux *= 3.
	xFiltered += aux.

	aux fillWith: self displaced: 1 @ -1.
	aux *= -3.
	xFiltered += aux.

	aux fillWith: self displaced: 1 @ 0.
	aux *= -10.
	xFiltered += aux.

	aux fillWith: self displaced: 1 @ 1.
	aux *= -3.
	xFiltered += aux.


	xFiltered *= xFiltered.
	yFiltered *= yFiltered.
	xFiltered += yFiltered.
	
	e _ xFiltered elements.
	1 to: e size do: [ :i | e at: i put: (e at: i) sqrt ].
	xFiltered border: (1@1 extent: xFiltered extent) with: 0.
	^xFiltered! !

!FloatImage methodsFor: 'filtering' stamp: 'jmv 10/19/2017 13:26:40'!
scharrNoDataValue: noDataValue dilatedBy: extraPixels avoidSaturatedPixels: aBoolean
	"See comment at #scharr
	Additionally, consider that pixels in us that are noDataValue should not contribute to a border.
	As Scharr uses 3x3 filters, this is 8 neighbors to each noDataValuePixels.
	If extraPixels is not zero, it is an extra border of that width to not consider borders. Useful, for example if images are interpolated and there are pixels that are neighbors of a noDataValue pixel, that as they are interpolated with noDataValue, they are invalid themselves. Usually just self extraPixels to zero
	if aBoolean is true, also consider saturated pixels as noData"

	| borders neighborhood mask |
	neighborhood _ 1 + extraPixels.
	borders _ self scharr.
	mask _ FloatImage extent: self extent.
	mask with: self replaceValues: [ :dummy :v |
		(v = noDataValue or: [aBoolean and: [v > 0.85]]) 
			ifTrue: [ 0 ]
			ifFalse: [ 1 ]].
	mask _ mask erodedBy: neighborhood.
	borders *= mask.
	^ borders! !

!FloatImage methodsFor: 'resampling' stamp: 'jmv 5/22/2015 12:27'!
shiftedLinearTransformedBy: anAffineTransformation resultExtent: resultExtentOrNil
	"Answer a new image.
	anAffineTransformation specifies the transformation from us to the result image.
	resultExtentOrNil ifNil: [ make it large enough ]

	Warning: there could be black areas in the result, and/or some image content in self could be lost.
	This depends on the arguments.

	Gives very good results for scale values not much smaller than 1.0. In such cases, prefer a filtering technique such
	as #lanczosTransformedBy:resultExtent: to avoid aliasing.

	See ShiftedLinearInterpolatingImage class comment.
	Not available in OpenCV.
	"
	| scaled interpolator resultHeight resultWidth p v inverseTransformation e |
	e _ resultExtentOrNil ifNil: [ self resultExtentFor: anAffineTransformation ].
	resultWidth _ e x.
	resultHeight _ e y.
	scaled _ self appropriateResultClass width: resultWidth height: resultHeight.	
	interpolator _ ShiftedLinearInterpolatingImage from: self.
	inverseTransformation _ anAffineTransformation inverseTransformation.
	1 to: resultHeight do: [ :y |
		1to: resultWidth do: [ :x |
			p _ inverseTransformation transform: x@y.
			v _ interpolator interpolatedValueAtX: p x y: p y.
			scaled x: x y: y put: v ]].
	^scaled! !

!FloatImage methodsFor: 'noise generation' stamp: 'jmv 4/8/2015 16:07'!
simulatePhotonNoise
	"Simulate photon noise, assuming we count electrons. See senders."

	| random |
	random _ NormalProbabilityDistribution new.
	self replaceValues: [ :i :j :v |
		(v max: 0) sqrt * random nextRandomNumber + v max: 0 ]! !

!FloatImage methodsFor: 'noise generation' stamp: 'jmv 4/6/2015 12:30'!
simulateReadNoise: electrons
	"Simulate Read Noise"

	self addNormalNoise: electrons! !

!FloatImage methodsFor: 'arithmetic-answer new' stamp: 'jmv 8/29/2014 10:58'!
sqrt

	^self copy replaceValues: [ :i :j :v | v sqrt ]! !

!FloatImage methodsFor: 'arithmetic-answer new' stamp: 'jmv 8/3/2012 15:14'!
squared
	"Answer the receiver multipled by itself."

	^self * self! !

!FloatImage methodsFor: 'statistics' stamp: 'jmv 7/19/2016 12:04:54'!
standardDeviation
	"
	See
		http://en.wikipedia.org/wiki/Variance
		http://www.mathsisfun.com/data/standard-deviation.html
	{600. 470. 170. 430. 300} standardDeviation
	"

	^elements standardDeviation! !

!FloatImage methodsFor: 'private' stamp: 'jmv 8/13/2012 16:52'!
validateSameSize: aFloatImage
	"For arithmetic between the receiver and the argument."

	(height = aFloatImage height and: [ width = aFloatImage width ]) ifFalse: [
		self error: 'Receiver and argument should be of the same size' ]! !

!FloatImage methodsFor: 'operations' stamp: 'jmv 8/23/2014 23:44'!
with: anotherImage replaceValues: aBlock
	| otherValues |
	self validateSameSize: anotherImage.
	otherValues _ anotherImage elements.
	1
		to: elements size
		do: [ :i |
			elements
				at: i
				put: (aBlock value: (elements at: i) value: (otherValues at: i)) ]! !

!FloatImage methodsFor: 'displaying' stamp: 'jmv 8/11/2017 14:21:35'!
withImaginary: aFloatImage displayMagnitudeAt: aPoint
	"Show magnitude of receiver and argument considered as a Complex image
	Usually prefer #withImaginary:displayMagnitudeLogAt:
	"

	| f imaginaryValues complex magnitude max |
	imaginaryValues _ aFloatImage elements.
	complex _ 1 + 1i.
	max _ elements max.
	f _ self as8bitFormEvaluating: [ :index |
		complex real: (elements at: index) imaginary: (imaginaryValues at: index).
		magnitude _ complex magnitude.
		(magnitude / max * 255 + 0.5) truncated min: 255 max: 0 ].
	f displayAt: aPoint! !

!FloatImage methodsFor: 'displaying' stamp: 'jmv 8/11/2017 14:22:03'!
withImaginary: aFloatImage displayMagnitudeLogAt: aPoint
	"Show logarithm of magnitude of receiver and argument considered as a Complex image"

	| f imaginaryValues complex magnitude max |
	imaginaryValues _ aFloatImage elements.
	complex _ 1 + 1i.
	max _ elements max log.
	f _ self as8bitFormEvaluating: [ :index |
		complex real: (elements at: index) imaginary: (imaginaryValues at: index).
		magnitude _ complex magnitude log.
		(magnitude / max * 255 + 0.5) truncated min: 255 max: 0 ].
	f displayAt: aPoint! !

!FloatImage methodsFor: 'displaying' stamp: 'jmv 1/9/2015 17:25'!
withImaginary: aFloatImage displayPhaseAt: aPoint
	"Show phase of receiver and argument considered as a Complex image"

	| f imaginaryValues complex twoPi phase |
	imaginaryValues _ aFloatImage elements.
	complex _ 1 + 1i.
	twoPi _ Float twoPi.
	f _ self as8bitFormEvaluating: [ :index |
		complex real: (elements at: index) imaginary: (imaginaryValues at: index).
		phase _ complex phase.
		(phase / twoPi + 0.5 * 255) rounded ].
	f displayAt: aPoint! !

!FloatImage methodsFor: 'filtering' stamp: 'jmv 9/29/2017 16:27:15'!
withTilesDo: aBlock tileSize: aNumber horizontalGuardZone: guardZoneWidth verticalGuardZone: guardZoneHeight trimResult: trimResult
	"
	Split receiver in tiles that are an integer power of 2 in height and width.
	Evaluate a block with each tile.
	For many operation that produce defects on the borders of the image (such as FFT-based, i.e. circular, convolution and deconvolution):
		guardZoneWidth: 		Number of pixels we are willing to lose on each side of a tile, to avoid border effects.
		guardZoneHeight:	Number of pixels we are willing to lose at top and bottom of a tile, to avoid border effects.
	Result image is smaller. Guard zones are used to avoid border effects in each tile, but also to avoid them in the result image. If trimResult is false, then keep original size, but leave a black border at the edges, covering the result guard zone.
	"
	| activeTileWidth activeTileHeight result tile verticalTileCount horizontalTileCount tileOriginInSelf activeResultHeight activeResultWidth tilePositionInResult |
	activeTileWidth _ aNumber - (2*guardZoneWidth).
	activeTileHeight _ aNumber - (2*guardZoneHeight).
	activeResultWidth _ self width - (guardZoneWidth * 2).
	activeResultHeight _ self height - (guardZoneHeight * 2).
	result _ FloatImage extent: (trimResult ifTrue: [activeResultWidth @ activeResultHeight ] ifFalse: [self extent]).

	tile _ FloatImage width: aNumber height: aNumber.
	verticalTileCount _ (activeResultHeight / activeTileHeight) ceiling.
	horizontalTileCount _ (activeResultWidth / activeTileWidth) ceiling.
	activeTileHeight _ (activeResultHeight / verticalTileCount) ceiling.
	activeTileWidth _ (activeResultWidth / horizontalTileCount) ceiling.
	
	1 to: verticalTileCount do: [ :iTile |
		1 to: horizontalTileCount do: [ :jTile |

			"Copy stuff to tile"
			tileOriginInSelf _ (jTile-1 * activeTileWidth +1) @ (iTile-1 * activeTileHeight +1).
			tile *= 0.	"Remove possible leftovers from last iteration"
			self
				copyToTile: tile
				position: tileOriginInSelf.

			"Do stuff"
			aBlock value: tile.

			"Grab results"
			tilePositionInResult _ trimResult ifTrue: [tileOriginInSelf - (guardZoneWidth@guardZoneHeight)] ifFalse: [tileOriginInSelf].
			result
				copyFromTile: tile
				position: tilePositionInResult
				leftGuardZone: guardZoneWidth
				topGuardZone: guardZoneHeight
				activewWidth: activeTileWidth
				activeHeight: activeTileHeight ]].
	^ result! !

!FloatImage methodsFor: 'file operations' stamp: 'jmv 3/11/2016 15:57'!
writeBMPfileNamed: fName

	self replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	self asForm writeBMPfileNamed: fName! !

!FloatImage methodsFor: 'file operations' stamp: 'jmv 5/31/2016 11:24'!
writeFileNamed: filename

	filename asFileEntry forceWriteStreamDo: [ :stream |
		(ReferenceStream on: stream)
			nextPut: self ]! !

!FloatImage methodsFor: 'file operations' stamp: 'jmv 4/28/2015 11:59'!
writePNGfileNamed: fName

	self replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	self asForm writePNGfileNamed: fName! !

!FloatImage methodsFor: 'accessing' stamp: 'jmv 8/23/2014 23:46'!
xyForMax
	"Find max"
	| index x y |
	index _ elements indexOfMax.
	x _ index - 1 \\ width + 1.
	y _ index - x / width + 1.
	^x@y
! !

!FloatImage class methodsFor: 'experiments' stamp: 'jmv 12/13/2016 10:46:40'!
blurEstimationExperiment

| answer aux bits blur blurred electronsPerDN estimation exposure f fullWell gap maxD maxDN original plotter r readNoiseElectrons sharpnessessX sharpnessessY xDer xDerDer xDerDerDer xDerDerDerDer zeros |
Display fill: (0@0 extent: 1900@800) fillColor: Color white.
gap _ 40.
fullWell _ 22050.
bits _ 8.
maxDN _ 1 << bits - 1.
electronsPerDN _ fullWell / maxDN.
readNoiseElectrons _ 43.	"Vita 25 specs say 2 DN10 22050/1024*2.0"
exposure _ 0.5.
exposure _ 0.1.
exposure _ 0.9.
exposure _ 0.3.
original _ FloatImage lena.
original _ original copy: (1@1 extent: 256@128).
aux _ FloatImage extent: original extent.
f _ fullWell*exposure.
maxD _ 20.
sharpnessessX _ FloatArray new: maxD.
sharpnessessY _ FloatArray new: maxD.
zeros _ FloatArray new: maxD.
"#(0.5 0.7 1 1.2 1.5 2 3 4 5 6 7 8 9 10 15 20 22 25 28 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100) withIndexDo: [ :b :blurIndex |"
"#(10 15 20 25 30 35 40) withIndexDo: [ :b :blurIndex |"
"#(1.5 2 3 4 5 6 ) withIndexDo: [ :b :blurIndex |"
"#(0.5 1 1.2 1.3 1.4 1.5) withIndexDo: [ :b :blurIndex |"
"#(0.5 1 1.2 1.3 1.4 1.5 2 4 6 8 10 20 30 40) withIndexDo: [ :b :blurIndex |"
"#(44 45 46 47 48 49 50 51 ) withIndexDo: [ :b :blurIndex |"
#(1 1.2 2 4 6 8 10) withIndexDo: [ :b :blurIndex |
	blur _ FloatMatrix newSize: original extent.
	blur fillWithGaussianKernelSigmaY: b sigmaX: 3*b/2; normalizeSum.

	"#convolveWith: no longer available. Check the suitability of #convolutionWith:"
	self halt: 'You need to check this code!!'.
"	blurred _ original copy convolveWith: blur copy."
	blurred _ original copy convolutionWith: blur copy.

	""
	blurred *= f.
	blurred simulatePhotonNoise.
	blurred simulateReadNoise: readNoiseElectrons.
	blurred /= f.
	""
	
	blurred displayWithGammaAt: (blurIndex-1*(256+gap))+2@0.
	1 to: maxD do: [ :d |
		sharpnessessX at: d put: (blurred acutanceAt: d@0 aux: aux).
	].
sharpnessessY _ sharpnessessX copy.
	xDer _ sharpnessessX derivative.
	xDerDer _ xDer derivative.
	xDerDerDer _ xDerDer derivative.
	xDerDerDerDer _ xDerDerDer derivative.
	plotter _ FFT new.
	r _ (blurIndex-1*(256+gap))+2@257 extent:256@512.
	plotter plot: zeros in: r color: Color green min: -0.01 max: 0.01.
	plotter verticalLine: b range: maxD in: r color: Color green.
	plotter plot: sharpnessessX in: r color: Color blue min: -0.1 max: 0.1.
	plotter plot: xDer in: r color: Color cyan min: -0.01 max: 0.01.
	estimation _ blurred blurEstimator.
	{3*b@b. blurred blurEstimator. estimation. sharpnessessX first. blurred acutance} print.
"	blurred blurEstimator."
	plotter verticalLine: blurred blurEstimator x range: maxD in: r color: Color blue.
"	plotter verticalLine: blurred blurEstimator y range: maxD in: r color: Color blue."
].
answer.
! !

!FloatImage class methodsFor: 'synthetic images' stamp: 'jmv 8/28/2012 17:24'!
checkerboard: extent n: n
	"Build and answer a checkerboard pattern
	(FloatImage checkerboard: 200@200 n: 8) display
	"
	| answer cellExtent |
	cellExtent _ extent // n.
	answer _ self width: cellExtent x * n height: cellExtent y * n.
	1 to: answer height do: [ :y |
		1 to: answer width do: [ :x |
			answer x: x y: y put: (x-1//cellExtent x) + (y-1//cellExtent y) +1 \\ 2 ]].
	^answer! !

!FloatImage class methodsFor: 'examples' stamp: 'jmv 4/28/2015 12:00'!
example3
	"Use a portion of display to show behavior on colored input.
	Try it with a real photo, results are much better
	Similar to BSplineInterpolatedImage>>example3 but using Lanczos.
	FloatImage example3
	"
	| original originalB originalG originalR scale bigger biggerB biggerG biggerR t |
	original _ Display copy: (0@0 extent: 60@60).
	scale _ 10.
"	original _ (Form fromFileNamed: 'IMG_2365.JPG') copy: (1100@1800 extent: 500@500)."
	originalR _ FloatImage from32BitColorForm: original component: #red.
	originalG _ FloatImage from32BitColorForm: original component: #green.
	originalB _ FloatImage from32BitColorForm: original component: #blue.
	original display.
	t _ AffineTransformation forImageZoom: scale.
	biggerR _ originalR lanczosTransformedBy: t resultExtent: nil.
	biggerG _ originalG lanczosTransformedBy: t resultExtent: nil.
	biggerB _ originalB lanczosTransformedBy: t resultExtent: nil.
	biggerR replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	biggerG replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	biggerB replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	bigger _ biggerR asFormG: biggerG b: biggerB.
	bigger display.
	original display! !

!FloatImage class methodsFor: 'examples' stamp: 'jmv 4/28/2015 12:01'!
example4
	"FOR SUBPIXEL RENDERED STUFF
	Use a portion of display to show behavior on colored input.
	Try it with a real photo, results are much better
	FloatImage example4
	Similar to BSplineInterpolatedImage>>example4 but using Lanczos.
	It is hard to pick a winner!!"
	| d bigger h origH origW original w biggerB biggerG biggerR originalB originalG originalR n scale filterScale xFilter yFilter |
	d _ 0.25.		"SubPixel separator"
	original _ Display copy: (0@0 extent: 60@60).
	scale _ 10.
	origW _ original width.
	origH _ original height.
	w _ origW * scale.
	h _ origH * scale.
	filterScale _ scale min: 1.
	n _ (4 / filterScale) floor +1.
	xFilter _ FloatArray new: n.
	yFilter _ FloatArray new: n.
"	original _ (Form fromFileNamed: 'IMG_2365.JPG') copy: (1100@1800 extent: 500@500)."
	originalR _ FloatImage from32BitColorForm: original component: #red.
	originalG _ FloatImage from32BitColorForm: original component: #green.
	originalB _ FloatImage from32BitColorForm: original component: #blue.
	original display.
	biggerR _ FloatImage width: w height: h.
	biggerG _ FloatImage width: w height: h.
	biggerB _ FloatImage width: w height: h.
	1 to: h do: [ :y |
		1 to: w do: [ :x |
			biggerR x: x y: y put: (originalR lanczosFilteredValueAtX: x-0.5-d/scale+0.5+d y: y-0.5/scale+0.5 filterScale: filterScale xFilterStore: xFilter yFilterStore: yFilter).
			biggerG x: x y: y put: (originalG lanczosFilteredValueAtX: x-0.5/scale+0.5 y: y-0.5/scale+0.5 filterScale: filterScale xFilterStore: xFilter yFilterStore: yFilter).
			biggerB x: x y: y put: (originalB lanczosFilteredValueAtX: x-0.5+d/scale+0.5-d y: y-0.5/scale+0.5 filterScale: filterScale xFilterStore: xFilter yFilterStore: yFilter) ]].
	biggerR replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	biggerG replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	biggerB replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	bigger _ biggerR asFormG: biggerG b: biggerB.
	bigger display.
	original display! !

!FloatImage class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 01:33'!
fromFileEntry: aFileEntry

	^aFileEntry readStreamDo: [ :stream |
		(ReferenceStream on: stream)
			next ]! !

!FloatImage class methodsFor: 'instance creation' stamp: 'jmv 4/22/2016 10:45'!
fromFileNamed: filename

	^ self fromFileEntry: filename asFileEntry! !

!FloatImage class methodsFor: 'instance creation' stamp: 'jmv 6/2/2016 20:33'!
fromForm: aForm
	aForm depth = 32 ifTrue: [
		^self from32BitColorForm: aForm component: #green ].

	(aForm is: #GrayForm) ifTrue: [
		^self fromGrayForm: aForm ].

	(aForm is: #ColorForm) ifTrue: [
		^self fromGrayForm: aForm ].

	self error: 'unsupported depth'! !

!FloatImage class methodsFor: 'synthetic images' stamp: 'jmv 2/22/2016 15:15'!
gradient: extent
	"Build and answer a gradient pattern
	(FloatImage gradient: 20@20) display
	"
	| answer |
	answer _ self extent: extent.
	1 to: extent y do: [ :y |
		1 to: extent x do: [ :x |
			answer x: x y: y put: x+y-2 / (extent x + extent y - 2) ]].
	^answer! !

!FloatImage class methodsFor: 'sample images' stamp: 'jmv 9/3/2015 15:18'!
grid: cellExtent extent: totalExtent
	"
	(FloatImage grid: 6@4 extent: 100@100) display
	"
	| answer |
	answer _ self extent: totalExtent.
	answer replaceValues: [ :i :j :v |
		(i / cellExtent y \\ 1 min: j / cellExtent x \\ 1)
		  	< 0.05 ifTrue: [0.1] ifFalse: [0.9]].
	^answer! !

!FloatImage class methodsFor: 'experiments' stamp: 'jmv 6/2/2016 20:33'!
interpolationExperiment1
	"
	self interpolationExperiment1
	
	Or play with this in a workspace
	"

	| original t e p |
	original _ FloatImage fromGrayForm: Cursor read asGrayForm.
	t _ AffineTransformation forImageRotation: -0.2 zoom: 10.23 originalExtent: original extent.
	e _ original resultExtentFor: t.
	p _ 0@0.
	original displayAt: p.
	p _ p + (original width@0).
	(original areaTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original bSplineTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original bicubicTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original bilinearTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original lanczosTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original nearestNeighborTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original shiftedLinearTransformedBy: t resultExtent: e) displayAt: p.! !

!FloatImage class methodsFor: 'experiments' stamp: 'jmv 1/27/2015 14:00'!
interpolationExperiment2
	"
	self interpolationExperiment2
	
	Or play with this in a workspace
	"

	| original t e p |
	original _ FloatImage checkerboard: 100@100 n: 25.
	t _ AffineTransformation forImageRotation: -0.2 zoom: 2.13 originalExtent: original extent.
	e _ original resultExtentFor: t.
	p _ 0@0.
	original displayAt: p.
	p _ p + (original width@0).
	(original areaTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original bSplineTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original bicubicTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original bilinearTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original lanczosTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original nearestNeighborTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original shiftedLinearTransformedBy: t resultExtent: e) displayAt: p.! !

!FloatImage class methodsFor: 'experiments' stamp: 'jmv 1/27/2015 14:01'!
interpolationExperiment3
	"
	self interpolationExperiment3
	
	Or play with this in a workspace
	"

	| original t e p |
	original _ FloatImage lena.
	t _ AffineTransformation forImageRotation: -0.2 zoom: 0.93 originalExtent: original extent.
	e _ original resultExtentFor: t.
	original displayAt: 0@256.
	p _ 0@0.
	(original areaTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original bSplineTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original bicubicTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original bilinearTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original lanczosTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original nearestNeighborTransformedBy: t resultExtent: e) displayAt: p.
	p _ p + (e x@0).
	(original shiftedLinearTransformedBy: t resultExtent: e) displayAt: p.! !

!FloatImage class methodsFor: 'useful functions' stamp: 'jmv 5/5/2013 22:32'!
lanczosFilter2: x
	"Evaluate the Lanczos filter for a=2.
	It is a (normalized) sinc modulated by the central lobe of a stretched sinc.
	Support is the width of the stretched central lobe, 2*a, in this case (-2 .. 2).
	Value is zero at nonzero integer x. Value is 1.0 for x = 0.
	n _ 512.
	r _ 10@20 extent: n@200.
	x0 _ -3.0.
	x1 _ 3.0.
	ary _ Array new: n.
	1 to: n do: [ :i |
		x _ i -1 / (n-1) * (x1-x0) + x0.
		f _ FloatImage lanczosFilter2: x.
		ary at: i put: f].
	FFT new plot: ary in: r.
	ary sum.
	1 to: 40 do: [ :nn |
		((nn to: n by: 1.53) inject: 0 into: [ :prevVal :i | prevVal + (ary at: i rounded)]) print ]
	"

	| scaledSinc xScaled stretchedSinc xStretched |
	xScaled _ Float pi * x.
	(x between: -2.0 and: 2.0)
		ifFalse: [ ^0.0 ].

	xScaled = 0.0
		ifTrue: [ ^ 1.0 ].
		
	scaledSinc _ xScaled sin / xScaled.
	xStretched _ xScaled / 2.0.
	stretchedSinc _ xStretched sin / xStretched.
	^scaledSinc * stretchedSinc! !

!FloatImage class methodsFor: 'instance creation' stamp: 'jmv 7/19/2016 15:06:20'!
meanAndStDevOfFolder: folderName baseName: base n: n extentOrNil: extentOrNil
	"
	| pair |
[
	pair _ FloatImage meanAndStDevOfFolder: 'C:\Users\Juan-Tuerca\ImagenesDeCalibracion\OriginalsRenamed\FM0001Micro' baseName: 'Gray' n: 19 extentOrNil: nil.
	pair first writePNGfileNamed: 'darkframe-scm-0-344-350-mean.png'.
	pair first writeFileNamed: 'darkframe-scm-0-344-350-mean.floatImage'.
	pair second writePNGfileNamed: 'darkframe-scm-0-344-350-stdev.png'.
	pair second writeFileNamed: 'darkframe-scm-0-344-350-stdev.floatImage'.
] forkAt: 35

	FloatImage fromFileNamed: 'darkframe-scm-0-344-350-mean.floatImage'
	"
	| folder form stdev mean each fileEntry count |
	mean _ FloatImage meanOfFolder: folderName baseName: base n: n extentOrNil: extentOrNil.
	folder _ folderName asDirectoryEntry.
	stdev _ FloatImage width: mean width height: mean height.
	each _ FloatImage width: mean width height: mean height.
	count _ 0.
	1 to: n do: [ :i |
		fileEntry _ folder // (base, (i printStringBase: 10 length: 2 padded: true), '.png').
		fileEntry exists ifTrue: [
			fileEntry print.
			form _ fileEntry formContents asGrayForm.
			each fillFromGrayForm: form.
			each -= mean.
			each *= each.
			stdev += each.
			count _ count + 1 ]].
	stdev replaceValues: [ :i :j :v | (v / count) sqrt ].
	^{ mean . stdev }! !

!FloatImage class methodsFor: 'instance creation' stamp: 'jmv 7/19/2016 15:07:50'!
meanOfFolder: folderName baseName: base n: n extentOrNil: extentOrNil
	"
	extentOrNil can be a Point to take the topLeft part of the images, or nil, to take the whole images.
	FloatImage meanOfFolder: 'DarkFrames' baseName: 'darkframe-scm-0-1-344-' n: 19
	FloatImage meanOfFolder: 'DarkFrames' baseName: 'darkframe-scm-0-1-344-' n: 3 extentOrNil:512@512
	"
	| folder form mean each count fileEntry |
	folder _ folderName asDirectoryEntry.
	"Build them now only if we know the extent"
	mean _ extentOrNil ifNotNil: [ FloatImage width: extentOrNil x height: extentOrNil y ].
	each _ extentOrNil ifNotNil: [ FloatImage width: extentOrNil x height: extentOrNil y ].
	count _ 0.
	1 to: n do: [ :i |
		fileEntry _ folder // (base, (i printStringBase: 10 length: 2 padded: true), '.png').
		fileEntry exists ifTrue: [
			fileEntry print.
			form _ fileEntry formContents asGrayForm.
			mean
				ifNil: [
					"First iteration, and they were not build before starting. Build them. Load first form into mean."
					mean _ FloatImage fromGrayForm: form.
					each _ FloatImage width: mean width height: mean height ]
				ifNotNil: [
					"Already built. Just add."
					each fillFromGrayForm: form.
					mean += each ].
			count _ count + 1 ]].
	mean *= (1.0/count).
	^mean! !

!FloatImage class methodsFor: 'testing' stamp: 'jmv 8/31/2015 11:52'!
testNoRotations
	"
	FloatImage testNoRotations

	See http://docs.opencv.org/modules/imgproc/doc/geometric_transformations.html#resize for OpenCV interpolation methods.
	"
	| c e original transformed t xx  yy |
	Transcript newLine.
	'Test resampling algorithms without scaling, rotation or translation.' print.
	'Any method not answering the original image is not a true interpolation.'  print.
	'To save you some time: The only one that fails this test is CV_INTER_AREA. It blurs the image and makes it "walk".'  print.

	c _ 10.

	xx _ 0.
	yy _ 0.
	original _ FloatImage lena.
	original displayAt: 0@0.
	Display getCanvas drawString: 'Original' at: xx@240 font: nil color: Color lightBrown embossed: true.
	e _ original extent.
	t _ (AffineTransformation withRadians: 0 ) scaledBy: 1/1.0.
	t _ (AffineTransformation withTranslation: e // 2) composedWith:
			(t composedWith:
					(AffineTransformation withTranslation: e // 2  negated)).

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Nearest Neighbor, like CV_INTER_NN (ms): '.
	c timesRepeat: [
		transformed _ transformed nearestNeighborTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed nearestNeighborTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Nearest Neighbor, like CV_INTER_NN' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Area, like CV_INTER_AREA (ms): '.
	c timesRepeat: [
		transformed _ transformed areaTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed areaTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Area, like CV_INTER_AREA' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Bilinear, like CV_INTER_LINEAR (ms): '.
	c timesRepeat: [
		transformed _ transformed bilinearTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed bilinearTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Bilinear, like CV_INTER_LINEAR' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) | true ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Oversampled Bilinear, unavailable in OpenCV (ms): '.
	c timesRepeat: [
		transformed _ transformed fourierDoubledBilinearTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed fourierDoubledBilinearTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Oversampled Bilinear, unavail.in OpenCV' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'BiCubic, like CV_INTER_CUBIC (ms): '.
	c timesRepeat: [
		transformed _ transformed bicubicTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed bicubicTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'BiCubic, like CV_INTER_CUBIC' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Lanczos, like INTER_LANCZOS4 (ms): '.
	c timesRepeat: [
		transformed _ transformed lanczosTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed lanczosTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Lanczos, like INTER_LANCZOS4' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'BSpline, unavailable in OpenCV (ms): '.
	c timesRepeat: [
		transformed _ transformed bSplineTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed bSplineTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'BSpline, unavailable in OpenCV' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Shifted Linear, unavailable in OpenCV (ms): '.
	c timesRepeat: [
		transformed _ transformed shiftedLinearTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed shiftedLinearTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Shifted Linear, unavail.in OpenCV' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	'=== The only one that fails this test is CV_INTER_AREA. It blurs the image and makes it "walk".'  print.! !

!FloatImage class methodsFor: 'testing' stamp: 'jmv 9/1/2015 10:48'!
testRotations
	"
	FloatImage testRotations

	See http://docs.opencv.org/modules/imgproc/doc/geometric_transformations.html#resize for OpenCV interpolation methods.
	"
	| a c e original transformed t xx  yy |
	Transcript newLine.
	'Test resampling algorithms without significant scaling.' print.
	'The approach is to do successive rotations.'  print.

	c _ 140.	"Evidencia la falta de nitidez de BiCubic, y tambien cierta deformación geométrica de Lanczos. BSpline es impresionante."
	c _ 27.
	a _ -360.0 / (c+1).

	xx _ 0.
	yy _ 0.
	original _ FloatImage lena.
	original displayAt: 0@0.
	Display getCanvas drawString: 'Original' at: xx@240 font: nil color: Color lightBrown embossed: true.
	e _ original extent.
	t _ (AffineTransformation withRadians: a/180*Float pi ) scaledBy: 1/1.0.
	t _ (AffineTransformation withTranslation: e // 2) composedWith:
			(t composedWith:
					(AffineTransformation withTranslation: e // 2  negated)).

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Nearest Neighbor, like CV_INTER_NN (ms): '.
	c timesRepeat: [
		transformed _ transformed nearestNeighborTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed nearestNeighborTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Nearest Neighbor, like CV_INTER_NN' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Area, like CV_INTER_AREA (ms): '.
	c timesRepeat: [
		transformed _ transformed areaTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed areaTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Area, like CV_INTER_AREA' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Bilinear, like CV_INTER_LINEAR (ms): '.
	c timesRepeat: [
		transformed _ transformed bilinearTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed bilinearTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Bilinear, like CV_INTER_LINEAR' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) | true ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Oversampled Bilinear, unavailable in OpenCV (ms): '.
	c timesRepeat: [
		transformed _ transformed fourierDoubledBilinearTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed fourierDoubledBilinearTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Oversampled Bilinear, unavail.in OpenCV' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'BiCubic, like CV_INTER_CUBIC (ms): '.
	c timesRepeat: [
		transformed _ transformed bicubicTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed bicubicTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'BiCubic, like CV_INTER_CUBIC' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Lanczos, like INTER_LANCZOS4 (ms): '.
	c timesRepeat: [
		transformed _ transformed lanczosTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed lanczosTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Lanczos, like INTER_LANCZOS4' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'BSpline, unavailable in OpenCV (ms): '.
	c timesRepeat: [
		transformed _ transformed bSplineTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed bSplineTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'BSpline, unavailable in OpenCV' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Shifted Linear, unavailable in OpenCV (ms): '.
	c timesRepeat: [
		transformed _ transformed shiftedLinearTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed shiftedLinearTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Shifted Linear, unavail.in OpenCV' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	'=== BSpline gives outstanding results.'print.
	'=== Next comes BiCubic and Oversampled Bilinear. Then and Lanczos.' print.
	'         (beware about Oversampled Bilinear: on larger images, It requres extra 4x memory, and FFT might make it slower than others)' print.
	'=== The extra cost of Lanczos doesnt seem worth in this case.' print.! !

!FloatImage class methodsFor: 'testing' stamp: 'jmv 9/1/2015 10:44'!
testTranslation
	"
	FloatImage testTranslation

	See http://docs.opencv.org/modules/imgproc/doc/geometric_transformations.html#resize for OpenCV interpolation methods.
	"
	| c e original transformed t xx  yy |
	Transcript newLine.
	'Test resampling algorithms without significant scaling.' print.
	'The approach is to do successive rotations.'  print.

	c _ 140.	"Evidencia la falta de nitidez de BiCubic, y tambien cierta deformación geométrica de Lanczos. BSpline es impresionante."
	c _ 27.

	xx _ 0.
	yy _ 0.
	original _ FloatImage lena.
	original displayAt: 0@0.
	Display getCanvas drawString: 'Original' at: xx@240 font: nil color: Color lightBrown embossed: true.
	e _ original extent.
	t _ AffineTransformation withTranslation: 15.0 / c @ (3.0/c).

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Nearest Neighbor, like CV_INTER_NN (ms): '.
	c timesRepeat: [
		transformed _ transformed nearestNeighborTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed nearestNeighborTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Nearest Neighbor, like CV_INTER_NN' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Area, like CV_INTER_AREA (ms): '.
	c timesRepeat: [
		transformed _ transformed areaTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed areaTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Area, like CV_INTER_AREA' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Bilinear, like CV_INTER_LINEAR (ms): '.
	c timesRepeat: [
		transformed _ transformed bilinearTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed bilinearTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Bilinear, like CV_INTER_LINEAR' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) | true ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Oversampled Bilinear, unavailable in OpenCV (ms): '.
	c timesRepeat: [
		transformed _ transformed fourierDoubledBilinearTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed fourierDoubledBilinearTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Oversampled Bilinear, unavail.in OpenCV' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'BiCubic, like CV_INTER_CUBIC (ms): '.
	c timesRepeat: [
		transformed _ transformed bicubicTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed bicubicTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'BiCubic, like CV_INTER_CUBIC' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Lanczos, like INTER_LANCZOS4 (ms): '.
	c timesRepeat: [
		transformed _ transformed lanczosTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed lanczosTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Lanczos, like INTER_LANCZOS4' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'BSpline, unavailable in OpenCV (ms): '.
	c timesRepeat: [
		transformed _ transformed bSplineTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed bSplineTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'BSpline, unavailable in OpenCV' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	xx _ xx + 256.
	xx > (Display width-256) ifTrue: [xx _ 0. yy _ yy + 256 ].
	transformed _ original.
	Transcript show: 'Shifted Linear, unavailable in OpenCV (ms): '.
	c timesRepeat: [
		transformed _ transformed shiftedLinearTransformedBy: t resultExtent: e.
		transformed displayAt: xx@yy ].
	Smalltalk garbageCollect.
	[ transformed _ transformed shiftedLinearTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Shifted Linear, unavail.in OpenCV' at: xx@(yy+240) font: nil color: Color lightBrown embossed: true.

	'=== BSpline gives outstanding results. Next comes Oversampled Bilinear, that is very fast.' print.
	'         (beware about Oversampled Bilinear: on larger images, It requres extra 4x memory, and FFT might make it slower than others)' print.
	'=== Then comes BiCubic and Lanczos. The extra cost of Lanczos doesnt seem worth in this case.' print.
	'=== Nearest Neighbor and Shifted Linear dont give consistent results across different transformations' print.! !

!FloatImage class methodsFor: 'testing' stamp: 'jmv 9/1/2015 10:44'!
testZoomIn
	"
	FloatImage testZoomIn

	See http://docs.opencv.org/modules/imgproc/doc/geometric_transformations.html#resize for OpenCV interpolation methods.
	"
	| original transformed t xx yy e scale |
	Transcript newLine.
	'Test image enlarging algorithms.' print.

	scale _ 2.7.
scale _ 2.
scale _ 2.7.
	xx _ 0.
	yy _ 0.
	original _ FloatImage lena copy: (100@100 extent: 100@100).
original _ FloatImage lena copy: (70@80 extent: 128@128).
	e _ (original extent * scale) rounded.
	original displayAt: 0@0.
	Display getCanvas drawString: 'Original' at: xx@240 font: nil color: Color lightBrown embossed: true.
	t _ AffineTransformation forImageZoom: scale.

	xx _ original  width.
	xx > (Display width-e x) ifTrue: [xx _ original  width. yy _ yy + e y ].
	transformed _ original.
	Transcript show: 'Nearest Neighbor, like CV_INTER_NN (ms): '.
	Smalltalk garbageCollect.
	[ transformed _ transformed nearestNeighborTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Nearest Neighbor, like CV_INTER_NN' at: xx@(yy+e y-20) font: nil color: Color lightBrown embossed: true.

	xx _ xx + e x.
	xx > (Display width-e x) ifTrue: [xx _ original  width. yy _ yy + e y ].
	transformed _ original.
	Transcript show: 'Area, like CV_INTER_AREA (ms): '.
	Smalltalk garbageCollect.
	[ transformed _ transformed areaTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Area, like CV_INTER_AREA' at: xx@(yy+e y-20) font: nil color: Color lightBrown embossed: true.

	xx _ xx + e x.
	xx > (Display width-e x) ifTrue: [xx _ original  width. yy _ yy + e y ].
	transformed _ original.
	Transcript show: 'Bilinear, like CV_INTER_LINEAR (ms): '.
	Smalltalk garbageCollect.
	[ transformed _ transformed bilinearTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Bilinear, like CV_INTER_LINEAR' at: xx@(yy+e y-20) font: nil color: Color lightBrown embossed: true.

	xx _ xx + e x.
	xx > (Display width-e x) | true ifTrue: [xx _ original  width. yy _ yy + e y ].
	transformed _ original copy guardZone: 2@2.
	Transcript show: 'Oversampled Bilinear, unavailable in OpenCV (ms): '.
	Smalltalk garbageCollect.
	[ transformed _ transformed fourierDoubledBilinearTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Oversampled Bilinear, unavail.in OpenCV' at: xx@(yy+e y-20) font: nil color: Color lightBrown embossed: true.

	xx _ xx + e x.
	xx > (Display width-e x) ifTrue: [xx _ original  width. yy _ yy + e y ].
	transformed _ original.
	Transcript show: 'BiCubic, like CV_INTER_CUBIC (ms): '.
	Smalltalk garbageCollect.
	[ transformed _ transformed bicubicTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'BiCubic, like CV_INTER_CUBIC' at: xx@(yy+e y-20) font: nil color: Color lightBrown embossed: true.

	xx _ xx + e x.
	xx > (Display width-e x) ifTrue: [xx _ original  width. yy _ yy + e y ].
	transformed _ original.
	Transcript show: 'Lanczos, like INTER_LANCZOS4 (ms): '.
	Smalltalk garbageCollect.
	[ transformed _ transformed lanczosTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Lanczos, like INTER_LANCZOS4' at: xx@(yy+e y-20) font: nil color: Color lightBrown embossed: true.

	xx _ xx + e x.
	xx > (Display width-e x) ifTrue: [xx _ original  width. yy _ yy + e y ].
	transformed _ original.
	Transcript show: 'BSpline, unavailable in OpenCV (ms): '.
	Smalltalk garbageCollect.
	[ transformed _ transformed bSplineTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'BSpline, unavailable in OpenCV' at: xx@(yy+e y-20) font: nil color: Color lightBrown embossed: true.

	xx _ xx + e x.
	xx > (Display width-e x) ifTrue: [xx _ original  width. yy _ yy + e y ].
	transformed _ original.
	Transcript show: 'Shifted Linear, unavailable in OpenCV (ms): '.
	Smalltalk garbageCollect.
	[ transformed _ transformed shiftedLinearTransformedBy: t resultExtent: e ] timeToRun print.
	transformed displayAt: xx@yy.
	Display getCanvas drawString: 'Shifted Linear, unavail.in OpenCV' at: xx@(yy+e y-20) font: nil color: Color lightBrown embossed: true.

	'=== Oversampled Bilinear gives outstanding best results' print.
	'         (beware about Oversampled Bilinear: on larger images, It requres extra 4x memory, and FFT might make it slower than others)' print.
	'=== BSpline gives great results. Next comes BiCubic, Lanczos and Bilinear.' print.! !

!FloatImage class methodsFor: 'testing' stamp: 'jmv 8/31/2015 13:36'!
testZoomOut
	"
	FloatImage testZoomOut

	See http://docs.opencv.org/modules/imgproc/doc/geometric_transformations.html#resize for OpenCV interpolation methods.
	"
	| original transformed t xx e scale |
	Transcript newLine.
	'Test subsampling algorithms.' print.

	scale _ 0.71.
	scale _ 0.371.
	scale _ 0.55.
	scale _ 0.25.
	xx _ 0.
	original _ FloatImage lena.
	e _ (original extent * scale) rounded.
	original displayAt: 0@0.
	Display getCanvas drawString: 'Original' at: xx@270 font: nil color: Color lightBrown embossed: true.
	t _ AffineTransformation forImageZoom: scale.

	xx _ xx + original  width.
	transformed _ original.
	Smalltalk garbageCollect.
	Transcript show: 'Nearest Neighbor, like CV_INTER_NN (ms): '.
	[ transformed _ transformed nearestNeighborTransformedBy: t resultExtent: nil ] timeToRun print.
	transformed displayAt: xx@0.
	Display getCanvas drawString: 'Nearest Neighbor, like CV_INTER_NN' at: xx@70 font: nil color: Color lightBrown embossed: true.
	
	xx _ xx + e x.
	transformed _ original.
	Smalltalk garbageCollect.
	Transcript show: 'Area, like CV_INTER_AREA (ms): '.
	[ transformed _ transformed areaTransformedBy: t resultExtent: nil ] timeToRun print.
	transformed displayAt: xx@0.
	Display getCanvas drawString: 'Area, like CV_INTER_AREA' at: xx@85 font: nil color: Color lightBrown embossed: true.

	xx _ xx + e x.
	transformed _ original.
	Smalltalk garbageCollect.
	Transcript show: 'Bilinear, like CV_INTER_LINEAR (ms): '.
	[ transformed _ transformed bilinearTransformedBy: t resultExtent: nil ] timeToRun print.
	transformed displayAt: xx@0.
	Display getCanvas drawString: 'Bilinear, like CV_INTER_LINEAR' at: xx@100 font: nil color: Color lightBrown embossed: true.

	xx _ xx + e x.
	transformed _ original.
	Smalltalk garbageCollect.
	Transcript show: 'Oversampled Bilinear, unavailable in OpenCV (ms): '.
	[ transformed _ transformed fourierDoubledBilinearTransformedBy: t resultExtent: nil ] timeToRun print.
	transformed displayAt: xx@0.
	Display getCanvas drawString: 'Oversampled Bilinear, unavail.in OpenCV' at: xx@115 font: nil color: Color lightBrown embossed: true.

	xx _ xx + e x.
	transformed _ original.
	Smalltalk garbageCollect.
	Transcript show: 'BiCubic, like CV_INTER_CUBIC (ms): '.
	[ transformed _ transformed bicubicTransformedBy: t resultExtent: nil ] timeToRun print.
	transformed displayAt: xx@0.
	Display getCanvas drawString: 'BiCubic, like CV_INTER_CUBIC' at: xx@130 font: nil color: Color lightBrown embossed: true.

	xx _ xx + e x.
	transformed _ original.
	Smalltalk garbageCollect.
	Transcript show: 'Lanczos, like INTER_LANCZOS4 (ms): '.
	[ transformed _ transformed lanczosTransformedBy: t resultExtent: nil ] timeToRun print.
	transformed displayAt: xx@0.
	Display getCanvas drawString: 'Lanczos, like INTER_LANCZOS4' at: xx@145 font: nil color: Color lightBrown embossed: true.

	xx _ xx + e x.
	transformed _ original.
	Smalltalk garbageCollect.
	Transcript show: 'BSpline, unavailable in OpenCV (ms): '.
	[ transformed _ transformed bSplineTransformedBy: t resultExtent: nil ] timeToRun print.
	transformed displayAt: xx@0.
	Display getCanvas drawString: 'BSpline, unavailable in OpenCV' at: xx@160 font: nil color: Color lightBrown embossed: true.

	xx _ xx + e x.
	transformed _ original.
	Smalltalk garbageCollect.
	Transcript show: 'Shifted Linear, unavailable in OpenCV (ms): '.
	[ transformed _ transformed shiftedLinearTransformedBy: t resultExtent: nil ] timeToRun print.
	transformed displayAt: xx@0.
	Display getCanvas drawString: 'Shifted Linear, unavailable in OpenCV' at: xx@175 font: nil color: Color lightBrown embossed: true.
	
	'=== Area is really good, and very very cheap.' print.
	'=== Lanczos gives best avoidance of aliasing effects but is quite expensive.' print.! !

!IrregularSampledImage methodsFor: 'accessing' stamp: 'jmv 2/25/2014 10:37'!
addVoronoiCellX: seedX y: seedY
	"Add a new Voronoi cell to the diagram
	x and y are integers in [1 .. width] and [1 .. height]"
	"Builds a Digital Voronoi diagram (digital because it is pixel based, i.e. discrete).
	The Voronoi diagram might not be perfect. No 'debris' points are added (see http://pub.ist.ac.at/~edels/Papers/2010-R-07-DigitalDelaunay2D.pdf )"

	| i chebyshevDist continue thisPoint oldCellSeed newCellSeed |
	"Add the new cell seed"
	i _ seedY-1 * width + seedX.
	closestKnownX at: i put: seedX.
	closestKnownY at: i put: seedY.

	"Update Voronoi diagram."
	newCellSeed _ seedX@seedY.
	chebyshevDist _ 0.
	continue _ true.
	[ continue ] whileTrue: [
		continue _ false.
		chebyshevDist _ chebyshevDist + 1.
		self pixelsAtChebyshevDistance: chebyshevDist ofX: seedX y: seedY do: [ :xx :yy :index |
			thisPoint _ xx@yy.
			oldCellSeed _ (closestKnownX at: index)@(closestKnownY at: index).
			((closestKnownX at: index) = 0 or: [ (thisPoint squaredDistanceTo: newCellSeed) < (thisPoint squaredDistanceTo: oldCellSeed) ]) ifTrue: [
				continue _ true.
				closestKnownX at: index put: seedX.
				closestKnownY at: index put: seedY ]]]! !

!IrregularSampledImage methodsFor: 'accessing' stamp: 'jmv 8/23/2014 23:56'!
clean
	"Zero values for all pixels except the seeds of the Voronoi cells"
	| i |
	1 to: height do: [ :yy |
		1 to: width do: [ :xx |
			i _ yy-1 * width + xx.
			((closestKnownX at: i) = xx and: [ (closestKnownY at: i) = yy ]) ifFalse: [
				elements at: i put: 0 ]]]! !

!IrregularSampledImage methodsFor: 'accessing' stamp: 'jmv 2/25/2014 10:24'!
delaunayTrianglesDo: aBlock
	"We already have a Voronoi diagram. Based on it, iterate over the Delaunay triangulation.
	To do this, iterate over all pixels, and for the points where 3 (or 4) Voronoi cells meet, we have one (or 2) Delaunay triangles."
	"see http://pub.ist.ac.at/~edels/Papers/2010-R-07-DigitalDelaunay2D.pdf "

	| cornerCount vertices pp p1 p2 p3 p4 |
	vertices _ Array new: 4.
	1 to: height do: [ :yy | 1 to: width do: [ :xx |
		pp _ xx@yy.
		cornerCount _ self voronoiCellsFor: pp on: vertices.
		cornerCount = 3 ifTrue: [
			p1 _ vertices first.
			p2 _ vertices second.
			p3 _ vertices third.
			aBlock value: p1 value: p2 value: p3 ].
		cornerCount = 4 ifTrue: [
			p1 _ vertices first.
			p2 _ vertices second.
			p3 _ vertices third.
			p4 _ vertices fourth.
			(p1 squaredDistanceTo: p3) < (p2 squaredDistanceTo: p4) ifTrue: [
				aBlock value: p1 value: p3 value: p2.
				aBlock value: p1 value: p3 value: p4 ]
			ifFalse: [
				aBlock value: p2 value: p4 value: p1.
				aBlock value: p2 value: p4 value: p3 ]]]]! !

!IrregularSampledImage methodsFor: 'initialization' stamp: 'jmv 8/23/2014 23:19'!
initializeElements

	| s |
	super initializeElements.
	s _ width * height roundUpTo: 2.
	closestKnownX _ ShortIntegerArray new: s.
	closestKnownY _ ShortIntegerArray new: s.
	possibleTriPoints _ OrderedCollection new! !

!IrregularSampledImage methodsFor: 'accessing' stamp: 'jmv 8/23/2014 23:54'!
interpolateBarycentric
	"Do a barycentric interpolation in each Delanuay triangle"
	| v x0 x1 y0 y1 v1 v2 v3 |
	self delaunayTrianglesDo:  [ :p1 :p2 :p3 |
		v1 _ self x: p1 x y: p1 y.
		v2 _ self x: p2 x y: p2 y.
		v3 _ self x: p3 x y: p3 y.
		y0 _ (p1 y min: p2 y) min: p3 y.
		y1 _ (p1 y max: p2 y) max: p3 y.
		x0 _ (p1 x min: p2 x) min: p3 x.
		x1 _ (p1 x max: p2 x) max: p3 x.
		y0 to: y1 do: [ :y |
			x0 to: x1 do: [ :x |
				x@y barycentricCoordinatesInTriangle: p1 with: p2 with: p3 into: [ :b1 :b2 :b3 |
					(b1 >= 0 and: [ b2 >= 0 and: [ b3 >= 0 ]]) ifTrue: [
						v _ (v1 * b1) + (v2 * b2) + (v3 * b3).
						self i: y j: x put: v ]]
				 ifDegenerate: []]]]! !

!IrregularSampledImage methodsFor: 'accessing' stamp: 'jmv 8/23/2014 23:54'!
interpolateBarycentricInLinearGammaSpace
	"Do a barycentric interpolation in each Delanuay triangle"
	| v x0 x1 y0 y1 v1 v2 v3 |
	self delaunayTrianglesDo:  [ :p1 :p2 :p3 |
		v1 _ self x: p1 x y: p1 y.
		v2 _ self x: p2 x y: p2 y.
		v3 _ self x: p3 x y: p3 y.
		v1 _ v1 raisedTo: 2.2.
		v2 _ v2 raisedTo: 2.2.
		v3 _ v3 raisedTo: 2.2.
		y0 _ (p1 y min: p2 y) min: p3 y.
		y1 _ (p1 y max: p2 y) max: p3 y.
		x0 _ (p1 x min: p2 x) min: p3 x.
		x1 _ (p1 x max: p2 x) max: p3 x.
		y0 to: y1 do: [ :y |
			x0 to: x1 do: [ :x |
				x@y barycentricCoordinatesInTriangle: p1 with: p2 with: p3 into: [ :b1 :b2 :b3 |
					(b1 >= 0 and: [ b2 >= 0 and: [ b3 >= 0 ]]) ifTrue: [
						v _ (v1 * b1) + (v2 * b2) + (v3 * b3).
						v _ v raisedTo: 1/2.2.
						self i: y j: x put: v ]]
				 ifDegenerate: []]]]! !

!IrregularSampledImage methodsFor: 'accessing' stamp: 'jmv 8/23/2014 23:54'!
interpolateNearestNeighbor
	"Simply fill each pixel with the value of the seed of its Voronoi cell.
	This is what most people expect as a Voronoi diagram"

	| seed |
	1 to: height do: [ :y |
		1 to: width do: [ :x |
			seed _ self voronoiCellSeedFor: x@y.
			self i: y j: x put: (self x: seed x y: seed y) ]]! !

!IrregularSampledImage methodsFor: 'accessing' stamp: 'jmv 2/18/2014 15:34'!
voronoiCellSeedFor: aPoint
	"Answer the point that is the seed for the cell we lie in"

	| i |
	i _ aPoint y-1 * width + aPoint x.
	^(closestKnownX at: i) @ (closestKnownY at: i)! !

!IrregularSampledImage methodsFor: 'testing' stamp: 'jmv 2/25/2014 11:00'!
voronoiCellsFor: aPoint on: anArray
	"Find the voronoi cells for aPoint together with its right, rightDown and down neighbors.
	Store them in anArray, and answer how many were found.
	Don't care about points that lie in a border of the image."

	| cell1 cell2 cell3 cell4 count |
	cell1 _ self voronoiCellSeedFor: aPoint.
	count _ 1.
	anArray at: count put: cell1.

	(aPoint between: 1@1 and: width@height -1) ifTrue: [

		cell2 _ self voronoiCellSeedFor: aPoint + (1@0).
		cell2 ~= cell1 ifTrue: [
			count _ count + 1.
			anArray at: count put: cell2 ].
	
		cell3 _ self voronoiCellSeedFor: aPoint + (1@1).
		(cell3 ~= cell1 and: [ cell3 ~= cell2 ]) ifTrue: [
			count _ count + 1.
			anArray at: count put: cell3 ].
	
		cell4 _ self voronoiCellSeedFor: aPoint + (0@1).
		(cell4 ~= cell1 and: [ cell4 ~= cell2 and: [ cell4 ~= cell3 ]]) ifTrue: [
			count _ count + 1.
			anArray at: count put: cell4 ].
		
		"Region necks are not valid Voronoi vertexes
		(see http://pub.ist.ac.at/~edels/Papers/2010-R-07-DigitalDelaunay2D.pdf )"
		(cell1 = cell3 or: [ cell2 = cell4 ]) ifTrue: [
			count _ 0 ].
	].

	^count! !

!IrregularSampledImage methodsFor: 'accessing' stamp: 'jmv 8/24/2014 00:18'!
x: x y: y put: aNumber
	"Set the pixel at coordinates x@y.
	x and y are integers in [1 .. width] and [1 .. height]"
	"Builds a Digital Voronoi diagram (digital because it is pixel based, i.e. discrete).
	Answer the last Chebisev distance evaluated. This is (at least) one pixel bigger that the new Voronoi cell.
	The Voronoi diagram might not be perfect. No 'debris' points are added (see http://pub.ist.ac.at/~edels/Papers/2010-R-07-DigitalDelaunay2D.pdf )"

	super x: x y: y put: aNumber.

	"Update Voronoi diagram."
	self addVoronoiCellX: x y: y! !

!IrregularSampledImage class methodsFor: 'testing' stamp: 'jmv 6/10/2016 12:52'!
test1
	"
	[ IrregularSampledImage test1 ] forkAt: 30
	"
	| h original r rebuilt w x y |
	original _ FloatImage lena.
	original _ original scaledBy: 0.5.
	w _ original width.
	h _ original height.
	rebuilt _ IrregularSampledImage width: w height: h.
	r _ Random new.
	r seed: 1.
	0 to: w*h// 4 do: [ :i |
		x _ r nextInteger: w.
		y _ r nextInteger: h.
		rebuilt x: x y: y put: (original x: x y: y).
		i \\ 100 = 0 ifTrue: [
			i print.
			rebuilt
				clean; displayAt: 0 @ 0;
				interpolateNearestNeighbor; displayAt: w @ 0;
				clean; interpolateBarycentric; displayAt: w*2 @ 0;
				clean; interpolateBarycentricInLinearGammaSpace; displayAt: w*3 @ 0.
			Processor yield ]]! !

!IrregularSampledImage class methodsFor: 'testing' stamp: 'jmv 6/10/2016 12:52'!
test2
	"
	[ IrregularSampledImage test2 ] forkAt: 30
	"
	| h original r rebuilt w x y |
	original _ FloatImage lena.
	original _ original scaledBy: 0.5.
	w _ original width.
	h _ original height.
	rebuilt _ IrregularSampledImage width: w height: h.
	r _ Random new.
	r seed: 1.
	0 to: 100 do: [ :i |
		x _ r nextInteger: w-24.
		y _ r nextInteger: h-16.
		0 to: 24 by:4 do: [ :xx |
			0 to: 16 by: 4 do: [ :yy |
				rebuilt x: x+xx y:y+ yy put: (original x: x+xx y: y+yy) ]].
		i \\ 1 = 0 ifTrue: [
			i print.
			rebuilt
				clean; displayAt: 0 @ 0;
				interpolateNearestNeighbor; displayAt: w @ 0;
				clean; interpolateBarycentric; displayAt: w*2 @ 0;
				clean; interpolateBarycentricInLinearGammaSpace; displayAt: w*3 @ 0 ]]! !

!IrregularSampledImage class methodsFor: 'testing' stamp: 'jmv 6/10/2016 12:53'!
test3
	"
	[ IrregularSampledImage test3 ] forkAt: 30
	"
	| h original rebuilt w ss |
	ss _ 2.
	original _ FloatImage lena.
	original _ original scaledBy: 0.5.
	w _ original width.
	h _ original height.
	rebuilt _ IrregularSampledImage width: w height: h.
	1 to: h by: ss do: [ :y |
		1 to: w by: ss do: [ :x |
			rebuilt x: x y: y put: (original x: x y: y) ].
		rebuilt
			clean; displayAt: 0 @ h;
			interpolateNearestNeighbor; displayAt: w @ h;
			clean; interpolateBarycentric; displayAt: w*2 @ h;
			clean; interpolateBarycentricInLinearGammaSpace; displayAt: w*3 @ h.
		Processor yield ]! !

!PerspectiveTransformation methodsFor: 'element access' stamp: 'jmv 1/13/2015 17:05'!
a0
	^self at: 1! !

!PerspectiveTransformation methodsFor: 'element access' stamp: 'jmv 1/13/2015 17:05'!
a1
	^self at: 2! !

!PerspectiveTransformation methodsFor: 'element access' stamp: 'jmv 1/13/2015 17:07'!
a2
	^self at: 3! !

!PerspectiveTransformation methodsFor: 'private access' stamp: 'jmv 1/13/2015 17:09'!
at: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!PerspectiveTransformation methodsFor: 'private access' stamp: 'jmv 1/13/2015 17:09'!
at: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value! !

!PerspectiveTransformation methodsFor: 'element access' stamp: 'jmv 1/13/2015 17:05'!
b0
	^self at: 4! !

!PerspectiveTransformation methodsFor: 'element access' stamp: 'jmv 1/13/2015 17:05'!
b1
	^self at: 5! !

!PerspectiveTransformation methodsFor: 'element access' stamp: 'jmv 1/13/2015 17:05'!
b2
	^self at: 6! !

!PerspectiveTransformation methodsFor: 'element access' stamp: 'jmv 1/13/2015 17:05'!
c0
	^self at: 7! !

!PerspectiveTransformation methodsFor: 'element access' stamp: 'jmv 1/13/2015 17:05'!
c1
	^self at: 8! !

!PerspectiveTransformation methodsFor: 'converting coordinates' stamp: 'jmv 1/13/2015 17:08'!
map: aPoint
	| xx yy zz |
	xx _ (self a0 * aPoint x) + (self a1 * aPoint y) + self a2.
	yy _ (self b0 * aPoint x) + (self b1 * aPoint y) + self b2.
	zz _ (self c0 * aPoint x) + (self c1 * aPoint y) + 1.
	^(xx / zz) @ (yy / zz)! !

!PerspectiveTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/13/2015 17:38'!
map: sourcePoints to: targetPoints
	"Both arguments must be of size 4.
	This method builds an instance, using an implementation of:
		http://docs.opencv.org/modules/imgproc/doc/geometric_transformations.html#Mat getPerspectiveTransform(InputArray src, InputArray dst)
	according to:
		http://math.stackexchange.com/questions/441597/how-to-calculate-what-matrix-will-transform-specified-points-to-other-specified
	
	(PerspectiveTransformation
		map: {3@5. 5@5. 2@2. 6@1}
		to: {0@3. 3@3. 0@0. 3@0})
		map: 3@5
	"
	
	| m x y u v elements result |
	m _ FloatMatrix m: 8 n: 9.
	1 to: 4 do: [ :i |
		x _ (sourcePoints at: i) x.
		y _ (sourcePoints at: i) y.
		u _ (targetPoints at: i) x.
		v _ (targetPoints at: i) y.
		m i: i j: 1 put: x.
		m i: i j: 2 put: y.
		m i: i j: 3 put: 1.
		m i: i j: 7 put: x negated * u.
		m i: i j: 8 put: y negated * u.
		m i: i j: 9 put: u.
		m i: i+4 j: 4 put: x.
		m i: i+4 j: 5 put: y.
		m i: i+4 j: 6 put: 1.
		m i: i+4 j: 7 put: x negated * v.
		m i: i+4 j: 8 put: y negated * v.
		m i: i+4 j: 9 put: v ].
	elements _ m solveLinearSystem.
	result _ self new.
	1 to: 8 do: [ :i | result at: i put: (elements at: i) ].
	^result! !

!PerspectiveTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/13/2015 17:04'!
new
	^(self basicNew: 8) initialize! !

!PerspectiveTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/13/2015 17:04'!
new: s
	self error: 'Please call PerspectiveTransformation>>#new (without arguments).'.
	^self new! !

!FloatImageTest methodsFor: 'testing' stamp: 'jmv 3/12/2017 19:33:09'!
testAreaInterpolation
	"
	FloatImageTest new testAreaInterpolation
	"
	| desiredResult large t subsampled |

	desiredResult _ FloatImage checkerboard: 8@8 n: 8.

	t _ AffineTransformation forImageZoom: 0.25.
	large _ FloatImage checkerboard: 32@32 n: 8.
	subsampled _ large areaTransformedBy: t resultExtent: nil.
	self assert: subsampled = desiredResult.

	t _ AffineTransformation forImageZoom: 1.0/3.0.
	large _ FloatImage checkerboard: 24@24 n: 8.
	subsampled _ large areaTransformedBy: t resultExtent: nil.
	self assert: subsampled = desiredResult.

	t _ AffineTransformation forImageZoom: 0.5.
	large _ FloatImage checkerboard: 16@16 n: 8.
	subsampled _ large areaTransformedBy: t resultExtent: nil.
	self assert: subsampled = desiredResult.

	t _ AffineTransformation forImageZoom: 1.0.
	large _ FloatImage checkerboard: 8@8 n: 8.
	subsampled _ large areaTransformedBy: t resultExtent: nil.
	self assert: subsampled = desiredResult.! !

!FloatImageTest methodsFor: 'testing' stamp: 'jmv 12/10/2012 17:09'!
testCentroids
	"Some test on the centroid functionality
	FloatImageTest new testCentroids
	This is the performance of the various centroid algorithms I tried
	centroidAround: 		0.03
	centroidAround4 	0.063
	centroidAround5 	0.25
	centroidAround1 	1.06
	centroidAround3 	2.06
	centroidAround2 	2.76
	"

	| testImage v c max |
	max _ 0.
	testImage _ FloatImage width: 5 height: 5.
	-0.5 to: 0.5 count: 21 do: [ :dx |
		-0.5 to: 0.5 count: 18 do: [ :dy |
			1.0 to: 5 count: 20 do: [ :scale |
				1 to: 5 do: [ :y |
					1 to: 5 do: [ :x |
						v _ (x@y - (dx@dy) - 3) r * Float pi / 2 /  scale.
						testImage x: x y: y put: v cos ]].
				"
				data _ testImage asFloatMatrix i: 3.
				FFT new plot: data elements in: (10@10 extent: 200@100).
				"
				c _ testImage centroidAround: 3@3.
				"We find centroid with error less than 3.5% of a pixel!!"
				self assert: (dx@dy + 3 - c) r < 0.0323.
				max _ max max: (dx@dy + 3 - c) r.
			]]].
	max print! !

!FloatImageTest methodsFor: 'testing' stamp: 'jmv 4/10/2015 16:23'!
testHistogram
	"
	FloatImageTest new testHistogram
	"
	| image histogram |
	image _ FloatImage fromArrayOfArrays: #(
		#(0.1   0.3   0.5   0.7   0.9 )
	).
	histogram _ image histogram: 5.
	self assert: (histogram countAt: 0.0) = 1.
	self assert: (histogram countAt: 1.0) = 1.
	self assert: (histogram accumulated countAt: 0.0) = 1.
	self assert: (histogram accumulated countAt: 1.0) = 5! !

!WaveletCodec methodsFor: 'subclass responsibilities' stamp: 'di 2/8/1999 14:22'!
bytesPerEncodedFrame
	"Answer the number of bytes required to hold one frame of compressed sound data. Answer zero if this codec produces encoded frames of variable size."

	^ 0
! !

!WaveletCodec methodsFor: 'subclass responsibilities' stamp: 'jmv 2/26/2016 17:02'!
decodeFrames: frameCount from: srcByteArray at: srcIndex into: dstSoundBuffer at: dstIndex
	"Decode the given number of monophonic frames starting at the given index in the given ByteArray of compressed sound data and storing the decoded samples into the given SoundBuffer starting at the given destination index. Answer a pair containing the number of bytes of compressed data consumed and the number of decompressed samples produced."
	"Note: Assume that the sender has ensured that the given number of frames will not exhaust either the source or destination buffers."

	| frameBase coeffArray scale i c nullCount samples sourceFrameEnd frameSize inStream val |
	inStream _ ReadStream on: srcByteArray from: srcIndex to: srcByteArray size.
	"frameCount _ " inStream nextUnsignedInt32BigEndian: true.
	samplesPerFrame _ inStream nextUnsignedInt32BigEndian: true.
	nLevels _ inStream nextUnsignedInt32BigEndian: true.
	alpha _ inStream nextFloat32BigEndian: true.
	beta _ inStream nextFloat32BigEndian: true.
	fwt ifNil:
		["NOTE: This should read parameters from the encoded data"
		fwt _ FWT new.
		fwt nSamples: samplesPerFrame nLevels: nLevels.
		fwt setAlpha: alpha beta: beta].
	frameBase _ dstIndex.
	coeffArray _ fwt coeffs.  "A copy that we can modify"

	1 to: frameCount do:
		[:frame | 

		"Decode the scale for this frame"
		frameSize _ inStream nextUnsignedInt16BigEndian: true.
		sourceFrameEnd _ frameSize + inStream position.
		scale _ inStream nextFloat32BigEndian: true.

		"Expand run-coded samples to scaled float values."
		i _ 5.
		[i <= coeffArray size]
			whileTrue:
			[c _ inStream next.
			c < 128
				ifTrue: [nullCount _ c < 112
							ifTrue: [c + 1]
							ifFalse: [(c-112)*256 + inStream next + 1].
						i to: i + nullCount - 1 do: [:j | coeffArray at: j put: 0.0].
						i _ i + nullCount]
				ifFalse: [val _ (c*256 + inStream next) - 32768 - 16384.
						coeffArray at: i put: val * scale.
						i _ i + 1]].

		"Copy float values into the wavelet sample array"		
			fwt coeffs: coeffArray.

		"Compute the transform"
		fwt transformForward: false.

		"Determine the scale for this frame"
		samples _ fwt samples.
		samples size = samplesPerFrame ifFalse: [self error: 'frame size error'].
		1 to: samples size do:
			[:j | dstSoundBuffer at: frameBase + j - 1 put: (samples at: j) asInteger].

		inStream position = sourceFrameEnd ifFalse: [self error: 'frame size error'].
		frameBase _ frameBase + samplesPerFrame].

	^ Array with: inStream position + 1 - srcIndex
			with: frameBase - dstIndex! !

!WaveletCodec methodsFor: 'subclass responsibilities' stamp: 'jmv 2/26/2016 17:13'!
encodeFrames: frameCount from: srcSoundBuffer at: srcIndex into: dstByteArray at: dstIndex
	"Encode the given number of frames starting at the given index in the given monophonic SoundBuffer and storing the encoded sound data into the given ByteArray starting at the given destination index. Encode only as many complete frames as will fit into the destination. Answer a pair containing the number of samples consumed and the number of bytes of compressed data produced."
	"Note: Assume that the sender has ensured that the given number of frames will not exhaust either the source or destination buffers."

	| frameBase coeffs maxVal minVal c scale nullCount frameI outFrameSize threshold outStream cMin val |
	threshold _ 2000.
	fwt ifNil:
		[samplesPerFrame _ self samplesPerFrame.
		nLevels _ 8.
		"Here are some sample mother wavelets, with the compression achieved on a
		sample of my voice at a threshold of 2000:
									compression achieved "
		alpha _ 0.0.  beta _ 0.0.		"12.1"
		alpha _ 1.72.  beta _ 1.51.	"14.0"
		alpha _ -1.86.  beta _ -1.53.	"14.4"
		alpha _ 1.28.  beta _ -0.86.	"15.9"
		alpha _ -1.15.  beta _ 0.69.	"16.0"
		fwt _ FWT new.
		fwt nSamples: samplesPerFrame nLevels: nLevels.
		fwt setAlpha: alpha beta: beta].

	(outStream _ WriteStream on: dstByteArray from: dstIndex to: dstByteArray size)
		nextUnsignedInt32Put: frameCount bigEndian: true;
		nextUnsignedInt32Put: samplesPerFrame bigEndian: true;
		nextUnsignedInt32Put: nLevels bigEndian: true;
		nextFloat32Put: alpha bigEndian: true;
		nextFloat32Put: beta bigEndian: true.
	frameBase _ srcIndex.
	1 to: frameCount do:
		[:frame | 

		"Copy float values into the wavelet sample array"		
		fwt samples: ((frameBase to: frameBase + samplesPerFrame-1) 
				collect: [:i | (srcSoundBuffer at: i) asFloat]).

		"Compute the transform"
		fwt transformForward: true.

		frameI _ outStream position+1.  "Reserve space for frame size"
		outStream nextUnsignedInt16Put: 0 bigEndian: true.

		"Determine and output the scale for this frame"
		coeffs _ fwt coeffs.
		maxVal _ 0.0.  minVal _ 0.0.
		5 to: coeffs size do:
			[:i | c _ coeffs at: i.
			c > maxVal ifTrue: [maxVal _ c].
			c < minVal ifTrue: [minVal _ c]].
		scale _ (maxVal max: minVal negated) / 16000.0.  "Will scale all to -16k..16k: 15 bits"
		outStream nextFloat32Put: scale bigEndian: true.

		"Copy scaled values, with run-coded sequences of 0's, to destByteArray"
		nullCount _ 0.
		cMin _ threshold / scale.
		5 to: coeffs size do:
			[:i | c _ (coeffs at: i) / scale.
			c abs < cMin
			ifTrue: ["Below threshold -- count nulls."
					nullCount _ nullCount + 1]
			ifFalse: ["Above threshold -- emit prior null count and this sample."
					nullCount > 0 ifTrue:
						[nullCount <= 112
						ifTrue: [outStream nextPut: nullCount-1]
						ifFalse: [outStream nextUnsignedInt16Put: (112*256) + nullCount-1 bigEndian: true ].
						nullCount _ 0].
						val _ c asInteger + 16384 + 32768.  "Map -16k..16k into 32k..64k"
						outStream nextUnsignedInt16Put: val bigEndian: true ]].

					nullCount > 0 ifTrue:
						[nullCount <= 112
						ifTrue: [outStream nextPut: nullCount-1]
						ifFalse: [outStream nextUnsignedInt16Put: (112*256) + nullCount-1 bigEndian: true ]].
		outFrameSize _ outStream position+1 - frameI - 2.  "Write frame size back at the beginning"
		(WriteStream on: dstByteArray from: frameI to: dstByteArray size)
			nextUnsignedInt16Put: outFrameSize bigEndian: true .
		frameBase _ frameBase + samplesPerFrame].

"This displays a temporary indication of compression achieved"
(((frameBase - srcIndex) *2.0 / (outStream position+1 - dstIndex) truncateTo: 0.1) printString , ' : 1') displayAt: Sensor mousePoint + (-20@30).

	outStream position > dstByteArray size ifTrue:
		["The calling routine only provides buffer space for compression of 2:1 or better.  If you are just testing things, you can increase it to, eg, codeFrameSize _ frameSize*3, which would be sufficient for a threshold of 0 (lossless conversion)."
		self error: 'Buffer overrun'].

	^ Array with: frameBase - srcIndex
			with: outStream position+1 - dstIndex! !

!WaveletCodec methodsFor: 'subclass responsibilities' stamp: 'jmv 2/26/2016 17:02'!
frameCount: aByteArray
	"Compute the frame count for this byteArray.  This default computation will have to be overridden by codecs with variable frame sizes."

	^ (ReadStream on: aByteArray) nextUnsignedInt32BigEndian: true! !

!WaveletCodec methodsFor: 'subclass responsibilities' stamp: 'di 2/8/1999 14:17'!
samplesPerFrame
	"Answer the number of sound samples per compression frame."

	^ 4096
! !

!AccumulatedImageHistogram methodsFor: 'accessing' stamp: 'jmv 4/24/2013 15:22'!
countAt: value
	"Even if we are not strictly 'counting'..."

	| n floatIndex i1 i2 |
	n _ tallies size.
	floatIndex _ (value * (n-1)) + 1.0.
	i1 _ floatIndex floor.
	i2 _ i1 + 1.
	i2 > n ifTrue: [
		^ tallies at: i1 ].
	^((tallies at: i1) * ( i2 - floatIndex)) + 
		((tallies at: i2) * (floatIndex - i1))! !

!AccumulatedImageHistogram methodsFor: 'initialization' stamp: 'jmv 4/24/2013 14:20'!
fillFrom: anImageHistogram

	| s histogramTallies |
	histogramTallies _ anImageHistogram tallies.
	s _ histogramTallies size + 1.
	tallies _ FloatArray new: s.
	tallies at: 1 put: 0.0.
	tallies at: 2 put: (histogramTallies at: 1).
	3 to: s do: [ :i |
		tallies at: i put: (histogramTallies at: i-1) + (tallies at: i-1) ]! !

!AccumulatedImageHistogram methodsFor: 'aux' stamp: 'jmv 4/24/2013 13:55'!
plot
	| rect |
	rect _ 10@10 extent: 768@400.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	FFT new plot: tallies in: rect color: Color black min: 0.0 max: tallies max! !

!AccumulatedImageHistogram methodsFor: 'aux' stamp: 'jmv 4/24/2013 15:19'!
plot2
	"Just in case there is some difference with #plot"
	| rect |
	rect _ 10@10 extent: 768@400.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	FFT new
		plot: ((0.0 to: 1.0 count: rect width) collect: [ :v | self countAt: v ])
		in: rect
		color: Color red
		min: 0.0
		max: tallies max! !

!AccumulatedImageHistogram methodsFor: 'aux' stamp: 'jmv 4/24/2013 13:55'!
plotIn: rect color: aColor min: min max: max
	FFT new plot: tallies in: rect color: aColor min: min max: max! !

!AccumulatedImageHistogram class methodsFor: 'instance creation' stamp: 'jmv 4/24/2013 14:20'!
from: anImageHistogram

	^self new fillFrom: anImageHistogram! !

!BSplineInterpolatingImage methodsFor: 'private' stamp: 'jmv 8/13/2012 17:00'!
coefficientAtX: x y: y

	^coefficients at: y+1 * (width+4) + x+2! !

!BSplineInterpolatingImage methodsFor: 'private' stamp: 'jmv 8/13/2012 16:32'!
coefficientAtX: x y: y put: aNumber

	^coefficients at: y+1 * (width+4) + x+2 put: aNumber! !

!BSplineInterpolatingImage methodsFor: 'pre-processing' stamp: 'jmv 8/13/2012 15:47'!
convertColumn: j
	"j belongs in (-1 to: width+2)"
	| lambda pole |
	
	"compute the overall gain"
	pole _ 3.0 sqrt - 2.0.
	lambda _ (1.0 - pole) * (1.0 - (1.0 / pole)).
	
	"apply the gain"
	-1 to: height+2 do: [ :i |
		self coefficientAtX: j y: i put: lambda * (self coefficientAtX: j y: i)].

	"causal initialization"
	self coefficientAtX: j y: -1 put: (self initialCausalCoefficientColumn: j pole: pole).
	"causal recursion"
	0 to: height+2 do: [ :i |
		self coefficientAtX: j y: i put: pole * (self coefficientAtX: j y: i-1) + (self coefficientAtX: j y: i)].

	"anticausal initialization"
	self coefficientAtX: j y: height+2 put: (self initialAntiCausalCoefficientColumn: j pole: pole).
	"anticausal recursion"
	height+1 to: -1 by: -1 do: [ :i |
		self coefficientAtX: j y: i put: pole * ((self coefficientAtX: j y: i+1) - (self coefficientAtX: j y: i)) ]! !

!BSplineInterpolatingImage methodsFor: 'pre-processing' stamp: 'jmv 8/13/2012 15:49'!
convertRow: i
	"i belongs in (-1 to: height+2)"
	| lambda pole |
	
	"compute the overall gain"
	pole _ 3.0 sqrt - 2.0.
	lambda _ (1.0 - pole) * (1.0 - (1.0 / pole)).
	
	"apply the gain"
	-1 to: width+2 do: [ :j |
		self coefficientAtX: j y: i put: lambda * (self coefficientAtX: j y: i)].
	
	"causal initialization"
	self coefficientAtX: -1 y: i put: (self initialCausalCoefficientRow: i pole: pole).
	"causal recursion"
	0 to: width+2 do: [ :j |
		self coefficientAtX: j y: i put: pole * (self coefficientAtX: j-1 y: i) + (self coefficientAtX: j y: i)].
	
	"anticausal initialization"
	self coefficientAtX: width+2 y: i put: (self initialAntiCausalCoefficientRow: i pole: pole).
	"anticausal recursion"
	width+1 to: -1 by: -1 do: [ :j |
		self coefficientAtX: j y: i put: pole * ((self coefficientAtX: j+1 y: i) - (self coefficientAtX: j y: i)) ]! !

!BSplineInterpolatingImage methodsFor: 'pre-processing' stamp: 'jmv 8/13/2012 15:50'!
initialAntiCausalCoefficientColumn: j pole: z
	"this initialization corresponds to mirror boundaries "
	^ z / (z * z - 1.0) * (z
		* (self coefficientAtX: j y: height + 1)
		+ (self coefficientAtX: j y: height + 2))! !

!BSplineInterpolatingImage methodsFor: 'pre-processing' stamp: 'jmv 8/13/2012 15:50'!
initialAntiCausalCoefficientRow: i pole: z 
	"this initialization corresponds to mirror boundaries"
	^ z / (z * z - 1.0) * (z
		* (self coefficientAtX: width + 1 y: i)
		+ (self coefficientAtX: width + 2 y: i))! !

!BSplineInterpolatingImage methodsFor: 'pre-processing' stamp: 'jmv 8/13/2012 15:52'!
initialCausalCoefficientColumn: j pole: z
	| zn sum iz z2n |

	"this initialization corresponds to mirror boundaries"
	zn _ z.
	iz _ 1.0 / z.
	z2n _ z raisedTo: height+1.
	sum _ (self coefficientAtX: j y: -1) + (z2n * (self coefficientAtX: j y: height+2)).
	z2n _ z2n * iz *z2n.
	0 to: height+1 do: [ :n |
		sum _ zn + z2n * (self coefficientAtX: j y: n) + sum.
		zn _ z * zn.
		z2n _ z2n * iz].
	^sum / (1.0 - (zn * zn))! !

!BSplineInterpolatingImage methodsFor: 'pre-processing' stamp: 'jmv 8/13/2012 15:53'!
initialCausalCoefficientRow: i pole: z
	| zn sum iz z2n |

	"this initialization corresponds to mirror boundaries"
	zn _ z.
	iz _ 1.0 / z.
	z2n _ z raisedTo: width+1.
	sum _ (self coefficientAtX: -1 y: i) + (z2n * (self coefficientAtX: width+2 y: i)).
	z2n _ z2n * iz *z2n.
	0 to: width+1 do: [ :n |
		sum _ zn + z2n * (self coefficientAtX: n y: i) + sum.
		zn _ z * zn.
		z2n _ z2n * iz].
	^sum / (1.0 - (zn * zn))! !

!BSplineInterpolatingImage methodsFor: 'pre-processing' stamp: 'jmv 8/13/2012 16:33'!
initializeFrom: aFloatImage

	| v |
	height _ aFloatImage height.
	width _ aFloatImage width.
	"We have two extra coefficients at each end. X indexes for them are -1, 0, width+1, with+2. Same for y."
	coefficients _ FloatArray new: (width+4) * (height+4).
	"Copy values. Add 2 extra rows and 2 extra columns at each edge, repeating boundary values."
	1 to: height do: [ :y |
		1 to: width do: [ :x |
			self coefficientAtX: x y: y put: (aFloatImage x: x y: y) ].
		v _ self coefficientAtX:1 y: y.
		self coefficientAtX: -1 y: y put: v.
		self coefficientAtX: 0 y: y put: v.
		v _ self coefficientAtX: width y: y.
		self coefficientAtX: width+1 y: y put: v.
		self coefficientAtX: width+2 y: y put: v ].
	-1 to: width+2 do: [ :x |
		v _ self coefficientAtX: x y: 1.
		self coefficientAtX: x y: -1 put: v.
		self coefficientAtX: x y: 0 put: v.
		v _ self coefficientAtX: x y: height.
		self coefficientAtX: x y: height + 1 put: v.
		self coefficientAtX: x y: height + 2 put: v ].

	-1 to: height+2 do: [ :i |
		self convertRow: i ].

	-1 to: width+2 do: [ :j |
		self convertColumn: j ]! !

!BSplineInterpolatingImage methodsFor: 'interpolating' stamp: 'jmv 5/4/2015 13:29'!
interpolatedValueAtX: x y: y
	"Evaluate the interpolated function. See class comment"

	| w interpolated xWeight4 xWeight1 xWeight3 xWeight2 yWeight4 yWeight1 yWeight3 yWeight2 xIndex2 xIndex1 xIndex3 xIndex4 yIndex2 yIndex1 yIndex3 yIndex4 oneSixth |
	"
	Podria ser que esto ocurra si los argumentos estan mal... habria que validar, pero no en cada evaluacion!!
	"
	self revisar.
"
(self xDomain rangeIncludes: x) ifFalse: [^0.0].
(self yDomain rangeIncludes: y) ifFalse: [ ^0.0].
"
	xIndex2 _ x floor.
	xIndex3 _ xIndex2+1.
	xIndex1 _ xIndex2-1.
	xIndex4 _ xIndex2+2.

	yIndex2 _ y floor.
	yIndex3 _ yIndex2+1.
	yIndex1 _ yIndex2-1.
	yIndex4 _ yIndex2+2.

	"Validate inside some reasonable bounds. We allow extending up to 0.8 in any direction"
	"x < 0.2 ifTrue: [ ^0.0 ]."			"Do it with integer arithmetic if at all possible"
	xIndex1 < -1 ifTrue: [ ^0.0 ].
	(xIndex1 = -1 and: [x < 0.2]) ifTrue: [ ^0.0 ].
	"x > (width + 0.8) ifTrue: [ ^0.0 ]."	"Do it with integer arithmetic if at all possible"
	xIndex1 > width ifTrue: [ ^0.0 ].
	(xIndex1 = width and: [x > (width + 0.8)]) ifTrue: [ ^0.0 ].
	"y < 0.2 ifTrue: [ ^0.0 ]."			"Do it with integer arithmetic if at all possible"
	yIndex1 < -1 ifTrue: [ ^0.0 ].
	(yIndex1 = -1 and: [y < 0.2]) ifTrue: [ ^0.0 ].
	"y > (height + 0.8) ifTrue: [ ^0.0 ]."	"Do it with integer arithmetic if at all possible"
	yIndex1 > height ifTrue: [ ^0.0 ].
	(yIndex1 = height and: [y > (height + 0.8)]) ifTrue: [ ^0.0 ].

	oneSixth _ 1.0 / 6.0.

	w _ x - xIndex2.
	xWeight4 _ oneSixth * w * w * w.
	xWeight1 _ oneSixth + ((w * w - w) * 0.5) - xWeight4.
	xWeight3 _ w + xWeight1 - (2.0 * xWeight4).
	xWeight2 _ 1.0 - (xWeight1 + xWeight3 + xWeight4).

	w _ y - yIndex2.
	yWeight4 _ oneSixth * w * w * w .
	yWeight1 _ oneSixth + ((w * w - w) * 0.5) - yWeight4.
	yWeight3 _ w + yWeight1 - (2.0 * yWeight4).
	yWeight2 _ 1.0 - (yWeight1 + yWeight3 + yWeight4).

	"/* perform interpolation */"
	w _ xWeight1 * (self coefficientAtX: xIndex1 y: yIndex1) +
		(xWeight2 * (self coefficientAtX: xIndex2 y: yIndex1)) +
		(xWeight3 * (self coefficientAtX: xIndex3 y: yIndex1)) +
		(xWeight4 * (self coefficientAtX: xIndex4 y: yIndex1)).
	interpolated _ yWeight1 * w.

	w _ xWeight1 * (self coefficientAtX: xIndex1 y: yIndex2) +
		(xWeight2 * (self coefficientAtX: xIndex2 y: yIndex2)) +
		(xWeight3 * (self coefficientAtX: xIndex3 y: yIndex2)) +
		(xWeight4 * (self coefficientAtX: xIndex4 y: yIndex2)).
	interpolated _ yWeight2 * w + interpolated.

	w _ xWeight1 * (self coefficientAtX: xIndex1 y: yIndex3) +
		(xWeight2 * (self coefficientAtX: xIndex2 y: yIndex3)) +
		(xWeight3 * (self coefficientAtX: xIndex3 y: yIndex3)) +
		(xWeight4 * (self coefficientAtX: xIndex4 y: yIndex3)).
	interpolated _ yWeight3 * w + interpolated.

	w _ xWeight1 * (self coefficientAtX: xIndex1 y: yIndex4) +
		(xWeight2 * (self coefficientAtX: xIndex2 y: yIndex4)) +
		(xWeight3 * (self coefficientAtX: xIndex3 y: yIndex4)) +
		(xWeight4 * (self coefficientAtX: xIndex4 y: yIndex4)).
	interpolated _ yWeight4 * w + interpolated.

	^interpolated! !

!BSplineInterpolatingImage class methodsFor: 'examples' stamp: 'jmv 9/2/2014 10:25'!
example1
	"Blue line shows samples joined by straight segments.
	Red line shows interpolation. It also shows how the function domain is enlarged by half a sample at each end. This is to ease zoomin into images, adding extra samples at the end in the same way that extra samples are added inside the image. This allows keeping visual aspect at the edges."
	| image interpolated interpolatedValues interpolatingXDomain max min n originalXDomain rect samples smallerRect |
	rect _ 100@20 extent: 600@210.
	samples _ { 1 . 2 . 3 . 1 }.
	samples _ { 1 . 2 . 3 . 2. 3. 5. 3. 7. 3. 3. 3. 4 . 1 . 1 .1 .1 .1 .1 .2 .5 .2 .5 .2 .5 .3 .3. 4}.
	n _ samples size.
	originalXDomain _ 1 to: n.
	"The interpolated function can be evaluated at the original points, and any point in between, and half a sample outside the original domain."
	interpolatingXDomain _ originalXDomain first - 0.5 to: originalXDomain last + 0.5 count: rect width.
	smallerRect _ rect left + (rect width / interpolatingXDomain extent * (originalXDomain first - interpolatingXDomain first) ) @ rect top
		extent: rect width / interpolatingXDomain extent * originalXDomain extent @ rect height.
	min _ samples min - 1.5.
	max _ samples max + 0.1.
	image _ FloatImage fromArrayOfArrays: { samples . samples . samples . samples . samples . samples}.
	interpolated _ BSplineInterpolatingImage from: image.

	interpolatedValues _ interpolatingXDomain collect: [ :x | interpolated interpolatedValueAtX: x y: 3 ].
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	FFT new plot: interpolatedValues in: rect color: Color red min: min max: max.
	FFT new plot: samples in: smallerRect color: Color blue min: min max: max.! !

!BSplineInterpolatingImage class methodsFor: 'examples' stamp: 'jmv 6/2/2016 20:31'!
example2
	"Zooming into a 1-bit black & white image is not the best, but it shows how this work.
	Note: resampling category in FloatImage has methods for whole image resampling."
	| bigger h interpolated origH origW original w |
	w _ 500.
	h _ 500.
	original _ FloatImage fromGrayForm: Cursor read asGrayForm.
	origW _ original width.
	origH _ original height.
	original display.
	interpolated _ BSplineInterpolatingImage from: original.
	bigger _ FloatImage width: w height: h.
	1 to: h do: [ :y |
		1 to: w do: [ :x |
			bigger x: x y: y put: (interpolated interpolatedValueAtX: x-0.5/w*origW+0.5 y: y-0.5/h*origH+0.5) ]].
	bigger display.
	original display! !

!BSplineInterpolatingImage class methodsFor: 'examples' stamp: 'jmv 4/28/2015 11:59'!
example3
	"Use a portion of display to show behavior on colored input.
	Try it with a real photo, results are much better
	BSplineInterpolatingImage example3
	"
	| original originalB originalG originalR bigger biggerB biggerG biggerR scale t |
	original _ Display copy: (0@0 extent: 100@100).
	scale _ 10.
"	original _ (Form fromFileNamed: 'IMG_2365.JPG') copy: (1100@1800 extent: 500@500)."
	originalR _ FloatImage from32BitColorForm: original component: #red.
	originalG _ FloatImage from32BitColorForm: original component: #green.
	originalB _ FloatImage from32BitColorForm: original component: #blue.
	original display.
	t _ AffineTransformation forImageZoom: scale.
	biggerR _ originalR bSplineTransformedBy: t resultExtent: nil.
	biggerG _ originalG bSplineTransformedBy: t resultExtent: nil.
	biggerB _ originalB bSplineTransformedBy: t resultExtent: nil.
	biggerR replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	biggerG replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	biggerB replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	bigger _ biggerR asFormG: biggerG b: biggerB.
	bigger display.
	original display! !

!BSplineInterpolatingImage class methodsFor: 'examples' stamp: 'jmv 4/28/2015 12:00'!
example4
	"FOR SUBPIXEL RENDERED STUFF
	Use a portion of display to show behavior on colored input.
	Simulate the effect of subpixel AA in the source image, and also do subpixel AA in the result"
	| bigger h origH origW original w biggerB biggerG biggerR interpolatedB interpolatedG interpolatedR originalB originalG originalR d |
	w _ 1000.
	h _ 1000.
	d _ 0.25.		"SubPixel separator"
	original _ Display copy: (0@0 extent: 100@100).
	originalR _ FloatImage from32BitColorForm: original component: #red.
	originalG _ FloatImage from32BitColorForm: original component: #green.
	originalB _ FloatImage from32BitColorForm: original component: #blue.
	origW _ original width.
	origH _ original height.
	original display.
	interpolatedR _ BSplineInterpolatingImage from: originalR.
	interpolatedG _ BSplineInterpolatingImage from: originalG.
	interpolatedB _ BSplineInterpolatingImage from: originalB.
	biggerR _ FloatImage width: w height: h.
	biggerG _ FloatImage width: w height: h.
	biggerB _ FloatImage width: w height: h.
	1 to: h do: [ :y |
		1 to: w do: [ :x |
			biggerR x: x y: y put: (interpolatedR interpolatedValueAtX: x-0.5-d/w*origW+0.5+d y: y-0.5/h*origH+0.5).
			biggerG x: x y: y put: (interpolatedG interpolatedValueAtX: x-0.5/w*origW+0.5 y: y-0.5/h*origH+0.5).
			biggerB x: x y: y put: (interpolatedB interpolatedValueAtX: x-0.5+d/w*origW+0.5-d y: y-0.5/h*origH+0.5) ]].
	biggerR replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	biggerG replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	biggerB replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	bigger _ biggerR asFormG: biggerG b: biggerB.
	bigger display.
	original display! !

!BSplineInterpolatingImage class methodsFor: 'instance creation' stamp: 'jmv 8/13/2012 15:17'!
from: aFloatImage

	^self new initializeFrom: aFloatImage! !

!ShiftedLinearInterpolatingImage methodsFor: 'private' stamp: 'jmv 3/22/2013 13:23'!
basisShift
	^0.5 - (1.0 / 12.0) sqrt.! !

!ShiftedLinearInterpolatingImage methodsFor: 'private' stamp: 'jmv 3/22/2013 15:24'!
coefficientAtX: x y: y

	^coefficients at: y-1 * width + x! !

!ShiftedLinearInterpolatingImage methodsFor: 'private' stamp: 'jmv 3/22/2013 15:24'!
coefficientAtX: x y: y put: aNumber

	^coefficients at: y-1 * width + x put: aNumber! !

!ShiftedLinearInterpolatingImage methodsFor: 'pre-processing' stamp: 'jmv 8/28/2015 16:08'!
convertColumn: j
	"j belongs in (1 to: width)"
	| factor pole |
	factor _ self factor.
	pole _ self pole.

	2 to: height do: [ :i |
		self coefficientAtX: j y: i put:
			(factor * (self coefficientAtX: j y: i)) + (pole * (self coefficientAtX: j y: i-1)) ]! !

!ShiftedLinearInterpolatingImage methodsFor: 'pre-processing' stamp: 'jmv 8/28/2015 16:08'!
convertRow: i
	"i belongs in (1 to: height)"
	| factor pole |
	factor _ self factor.
	pole _ self pole.

	2 to: width do: [ :j |
		self coefficientAtX: j y: i put:
			(factor * (self coefficientAtX: j y: i)) + (pole * (self coefficientAtX: j-1 y: i)) ]! !

!ShiftedLinearInterpolatingImage methodsFor: 'private' stamp: 'jmv 8/28/2015 16:08'!
factor
	"
	ShiftedLinearInterpolatingImage factor
	"
	^1.0 / (1.0 - self basisShift)! !

!ShiftedLinearInterpolatingImage methodsFor: 'pre-processing' stamp: 'jmv 8/23/2014 23:44'!
initializeFrom: aFloatImage

	height _ aFloatImage height.
	width _ aFloatImage width.
	coefficients _ aFloatImage elements copy.

	1 to: height do: [ :i |
		self convertRow: i ].

	1 to: width do: [ :j |
		self convertColumn: j ]! !

!ShiftedLinearInterpolatingImage methodsFor: 'interpolating' stamp: 'jmv 8/31/2015 11:40'!
interpolatedValueAtX: xx y: yy
	"Evaluate the interpolated function. See class comment"

	| w interpolated xWeight1 xWeight0 yWeight1 yWeight0 xIndex0 xIndex1 yIndex0 yIndex1 basisShift x y xIndexCheck yIndexCheck |

"
(self xDomain rangeIncludes: x) ifFalse: [^0.0].
(self yDomain rangeIncludes: y) ifFalse: [ ^0.0].
"
	basisShift _ self basisShift.
	x _ xx - basisShift.
	y _ yy - basisShift.

	xIndex0 _ x floor.
	xIndex1 _ xIndex0+1.
	yIndex0 _ y floor.
	yIndex1 _ yIndex0+1.

	"Validate inside some reasonable bounds. We allow extending up to 0.8 in any direction"
	"x < 0.2 ifTrue: [ ^0.0 ]."			"Do it with integer arithmetic if at all possible"
	xIndex0 < 0 ifTrue: [ ^0.0 ].
	(xIndex0 = 0 and: [x < 0.2]) ifTrue: [ ^0.0 ].
	"x > (width + 0.8) ifTrue: [ ^0.0 ]."	"Do it with integer arithmetic if at all possible"
	xIndexCheck _ xIndex0 - 1.
	xIndexCheck > width ifTrue: [ ^0.0 ].
	(xIndexCheck = width and: [x > (width + 0.8)]) ifTrue: [ ^0.0 ].
	"y < 0.2 ifTrue: [ ^0.0 ]."			"Do it with integer arithmetic if at all possible"
	yIndex0 < 0 ifTrue: [ ^0.0 ].
	(yIndex0 = 0 and: [y < 0.2]) ifTrue: [ ^0.0 ].
	"y > (height + 0.8) ifTrue: [ ^0.0 ]."	"Do it with integer arithmetic if at all possible"
	yIndexCheck _ yIndex0 - 1.
	yIndexCheck > height ifTrue: [ ^0.0 ].
	(yIndexCheck = height and: [y > (height + 0.8)]) ifTrue: [ ^0.0 ].

	xWeight1 _ x - xIndex0.
	xWeight0 _ 1.0 - xWeight1.
	yWeight1 _ y - yIndex0.
	yWeight0 _ 1.0 - yWeight1.

	xIndex0 _ xIndex0 min: width max: 1.
	xIndex1 _ xIndex1 min: width.
	yIndex0 _ yIndex0 min: height max: 1.
	yIndex1 _ yIndex1 min: height.

	"/* perform interpolation */"
	w _ (xWeight0 * (self coefficientAtX: xIndex0 y: yIndex0)) +
		(xWeight1 * (self coefficientAtX: xIndex1 y: yIndex0)).
	interpolated _ yWeight0 * w.

	w _ (xWeight0 * (self coefficientAtX: xIndex0 y: yIndex1)) +
		(xWeight1 * (self coefficientAtX: xIndex1 y: yIndex1)).
	interpolated _ yWeight1 * w + interpolated.

	^interpolated! !

!ShiftedLinearInterpolatingImage methodsFor: 'private' stamp: 'jmv 8/28/2015 16:08'!
pole
	"
	ShiftedLinearInterpolatingImage pole
	"
	^self basisShift / (self basisShift - 1.0)! !

!ShiftedLinearInterpolatingImage class methodsFor: 'examples' stamp: 'jmv 9/2/2014 10:24'!
example1
	"Blue line shows samples joined by straight segments.
	Red line shows interpolation. It also shows how the function domain is enlarged by half a sample at each end. This is to ease zoomin into images, adding extra samples at the end in the same way that extra samples are added inside the image. This allows keeping visual aspect at the edges."
	| image interpolated interpolatedValues interpolatingXDomain max min n originalXDomain rect samples smallerRect |
	rect _ 100@20 extent: 600@210.
	samples _ { 1 . 2 . 3 . 1 }.
	samples _ { 1 . 2 . 3 . 2. 3. 5. 3. 7. 3. 3. 3. 4 . 1 . 1 .1 .1 .1 .1 .2 .5 .2 .5 .2 .5 .3 .3. 4}.
	n _ samples size.
	originalXDomain _ 1 to: n.
	"The interpolated function can be evaluated at the original points, and any point in between, and half a sample outside the original domain."
	interpolatingXDomain _ originalXDomain first - 0.5 to: originalXDomain last + 0.5 count: rect width.
	smallerRect _ rect left + (rect width / interpolatingXDomain extent * (originalXDomain first - interpolatingXDomain first) ) @ rect top
		extent: rect width / interpolatingXDomain extent * originalXDomain extent @ rect height.
	min _ samples min - 1.5.
	max _ samples max + 0.1.
	image _ FloatImage fromArrayOfArrays: { samples . samples . samples . samples . samples . samples}.
	interpolated _ ShiftedLinearInterpolatingImage from: image.

	interpolatedValues _ interpolatingXDomain collect: [ :x | interpolated interpolatedValueAtX: x y: 3 ].
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	FFT new plot: interpolatedValues in: rect color: Color red min: min max: max.
	FFT new plot: samples in: smallerRect color: Color blue min: min max: max.! !

!ShiftedLinearInterpolatingImage class methodsFor: 'examples' stamp: 'jmv 6/2/2016 20:35'!
example2
	"Zooming into a 1-bit black & white image is not the best, but it shows how this work.
	Note: resampling category in FloatImage has methods for whole image resampling."
	| bigger h interpolated origH origW original w |
	w _ 500.
	h _ 500.
	original _ FloatImage fromGrayForm: Cursor read asGrayForm.
	origW _ original width.
	origH _ original height.
	original display.
	interpolated _ ShiftedLinearInterpolatingImage from: original.
	bigger _ FloatImage width: w height: h.
	1 to: h do: [ :y |
		1 to: w do: [ :x |
			bigger x: x y: y put: (interpolated interpolatedValueAtX: x-0.5/w*origW+0.5 y: y-0.5/h*origH+0.5) ]].
	bigger display.
	original display! !

!ShiftedLinearInterpolatingImage class methodsFor: 'examples' stamp: 'jmv 4/28/2015 12:02'!
example3
	"Use a portion of display to show behavior on colored input.
	Try it with a real photo, results are much better
	ShiftedLinearInterpolatingImage example3
	"
	| original originalB originalG originalR bigger biggerB biggerG biggerR scale t |
	original _ Display copy: (0@0 extent: 100@100).
	scale _ 10.
"	original _ (Form fromFileNamed: 'IMG_2365.JPG') copy: (1100@1800 extent: 500@500)."
	originalR _ FloatImage from32BitColorForm: original component: #red.
	originalG _ FloatImage from32BitColorForm: original component: #green.
	originalB _ FloatImage from32BitColorForm: original component: #blue.
	original display.
	t _ AffineTransformation forImageZoom: scale.
	biggerR _ originalR shiftedLinearTransformedBy: t resultExtent: nil.
	biggerG _ originalG shiftedLinearTransformedBy: t resultExtent: nil.
	biggerB _ originalB shiftedLinearTransformedBy: t resultExtent: nil.
	biggerR replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	biggerG replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	biggerB replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	bigger _ biggerR asFormG: biggerG b: biggerB.
	bigger display.
	original display! !

!ShiftedLinearInterpolatingImage class methodsFor: 'examples' stamp: 'jmv 4/28/2015 12:03'!
example4
	"FOR SUBPIXEL RENDERED STUFF
	Use a portion of display to show behavior on colored input.
	ShiftedLinearInterpolatingImage example4
	Simulate the effect of subpixel AA in the source image, and also do subpixel AA in the result"
	| bigger h origH origW original w biggerB biggerG biggerR interpolatedB interpolatedG interpolatedR originalB originalG originalR d |
	w _ 1000.
	h _ 1000.
	d _ 0.25.		"SubPixel separator"
	original _ Display copy: (0@0 extent: 100@100).
	originalR _ FloatImage from32BitColorForm: original component: #red.
	originalG _ FloatImage from32BitColorForm: original component: #green.
	originalB _ FloatImage from32BitColorForm: original component: #blue.
	origW _ original width.
	origH _ original height.
	original display.
	interpolatedR _ ShiftedLinearInterpolatingImage from: originalR.
	interpolatedG _ ShiftedLinearInterpolatingImage from: originalG.
	interpolatedB _ ShiftedLinearInterpolatingImage from: originalB.
	biggerR _ FloatImage width: w height: h.
	biggerG _ FloatImage width: w height: h.
	biggerB _ FloatImage width: w height: h.
	1 to: h do: [ :y |
		1 to: w do: [ :x |
			biggerR x: x y: y put: (interpolatedR interpolatedValueAtX: x-0.5-d/w*origW+0.5+d y: y-0.5/h*origH+0.5).
			biggerG x: x y: y put: (interpolatedG interpolatedValueAtX: x-0.5/w*origW+0.5 y: y-0.5/h*origH+0.5).
			biggerB x: x y: y put: (interpolatedB interpolatedValueAtX: x-0.5+d/w*origW+0.5-d y: y-0.5/h*origH+0.5) ]].
	biggerR replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	biggerG replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	biggerB replaceValues:  [ :i :j :v | v min: 255.0/256.0 max: 0.0 ].
	bigger _ biggerR asFormG: biggerG b: biggerB.
	bigger display.
	original display! !

!ShiftedLinearInterpolatingImage class methodsFor: 'instance creation' stamp: 'jmv 8/13/2012 15:17'!
from: aFloatImage

	^self new initializeFrom: aFloatImage! !

!ImageHistogram methodsFor: 'operations' stamp: 'jmv 7/24/2012 18:15'!
accumulate

	2 to: tallies size do: [ :i |
		tallies at: i put: (tallies at: i) + (tallies at: i-1) ]! !

!ImageHistogram methodsFor: 'operations' stamp: 'jmv 4/24/2013 15:24'!
accumulated

	^self copyTwoLevel accumulate! !

!ImageHistogram methodsFor: 'building' stamp: 'jmv 7/24/2012 17:56'!
addValue: aNumber

	| i |
	i _ self indexFor: aNumber.
	tallies at: i put: (tallies at: i) + 1! !

!ImageHistogram methodsFor: 'accessing' stamp: 'jmv 7/24/2012 18:21'!
countAt: value
	^tallies at: (self indexFor: value)! !

!ImageHistogram methodsFor: 'private' stamp: 'jmv 7/24/2012 18:12'!
indexFor: aValue
	| slotIndex |
	slotIndex _ (aValue * (tallies size-1)) rounded + 1.
	^slotIndex min: tallies size max: 1.! !

!ImageHistogram methodsFor: 'initialization' stamp: 'jmv 7/24/2012 16:38'!
initialize: anInteger

	tallies _ WordArray new: anInteger! !

!ImageHistogram methodsFor: 'aux' stamp: 'jmv 4/6/2015 11:39'!
plot
	| rect |
	rect _ 10@10 extent: 768@400.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	FFT new plot: tallies in: rect color: Color black min: 0.0 max: tallies max! !

!ImageHistogram methodsFor: 'aux' stamp: 'jmv 7/26/2012 15:49'!
plotIn: rect color: aColor min: min max: max
FFT new plot: tallies in: rect color: aColor min: min max: max! !

!ImageHistogram class methodsFor: 'instance creation' stamp: 'jmv 7/24/2012 16:35'!
new: anInteger

	^self new initialize: anInteger! !

!ImageHistogram2 methodsFor: 'operations' stamp: 'jmv 11/5/2012 14:19'!
accumulate

	2 to: tallies size do: [ :i |
		tallies at: i put: (tallies at: i) + (tallies at: i-1) ]! !

!ImageHistogram2 methodsFor: 'operations' stamp: 'jmv 4/24/2013 15:24'!
accumulated

	^self copyTwoLevel accumulate! !

!ImageHistogram2 methodsFor: 'building' stamp: 'jmv 11/5/2012 14:43'!
addValue: aValue

	| n floatIndex i1 i2 |
	n _ tallies size.

	floatIndex _ (aValue * (n-1)) + 1.0.
	floatIndex <= 1.0 ifTrue: [
		tallies at: 1 put: (tallies at: 1) + 1.0.
		^self ].

	floatIndex >= n ifTrue: [
		tallies at: n put: (tallies at: n) + 1.0.
		^self ].

	i1 _ floatIndex floor.
	i2 _ i1 + 1.
	tallies at: i1 put: (tallies at: i1) +( i2 - floatIndex).
	tallies at: i2 put: (tallies at: i2) + (floatIndex - i1)! !

!ImageHistogram2 methodsFor: 'accessing' stamp: 'jmv 11/5/2012 14:54'!
countAt: value
	"Even if we are not strictly 'counting'..."

	| n floatIndex i1 i2 |
	n _ tallies size.

	floatIndex _ (value * (n-1)) + 1.0.
	floatIndex <= 1.0 ifTrue: [
		^tallies at: 1 ].

	floatIndex >= n ifTrue: [
		^tallies at: n ].

	i1 _ floatIndex floor.
	i2 _ i1 + 1.
	^((tallies at: i1) * ( i2 - floatIndex)) + 
		((tallies at: i2) * (floatIndex - i1))! !

!ImageHistogram2 methodsFor: 'initialization' stamp: 'jmv 11/5/2012 14:30'!
initialize: anInteger

	tallies _ FloatArray new: anInteger! !

!ImageHistogram2 methodsFor: 'aux' stamp: 'jmv 4/24/2013 11:20'!
plot
	| rect |
	rect _ 10@10 extent: 768@400.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	FFT new plot: tallies in: rect color: Color black min: 0.0 max: tallies max! !

!ImageHistogram2 methodsFor: 'aux' stamp: 'jmv 4/24/2013 11:36'!
plot2
	"Just in case there is some difference with #plot"
	| rect |
	rect _ 10@10 extent: 768@400.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	FFT new
		plot: ((0.0 to: 1.0 count: rect width) collect: [ :v | self countAt: v ])
		in: rect
		color: Color red
		min: 0.0
		max: tallies max! !

!ImageHistogram2 methodsFor: 'aux' stamp: 'jmv 11/5/2012 14:30'!
plotIn: rect color: aColor min: min max: max
	FFT new plot: tallies in: rect color: aColor min: min max: max! !

!ImageHistogram2 class methodsFor: 'instance creation' stamp: 'jmv 11/5/2012 14:19'!
new: anInteger

	^self new initialize: anInteger! !

!ImageHistogram3 methodsFor: 'operations' stamp: 'jmv 4/24/2013 14:23'!
accumulated

	^AccumulatedImageHistogram from: self! !

!ImageHistogram3 methodsFor: 'building' stamp: 'jmv 4/24/2013 13:44'!
addValue: aValue

	| n floatIndex i1 i2 |
	n _ tallies size.

	floatIndex _ (aValue * n) + 0.5.
	i1 _ floatIndex floor.
	i2 _ i1 + 1.

	i1 < 1 ifTrue: [
		tallies at: 1 put: (tallies at: 1) + 1.
		^self ].

	i2 > n ifTrue: [
		tallies at: n put: (tallies at: n) + 1.
		^self ].

	tallies at: i1 put: (tallies at: i1) +( i2 - floatIndex).
	tallies at: i2 put: (tallies at: i2) + (floatIndex - i1)! !

!ImageHistogram3 methodsFor: 'accessing' stamp: 'jmv 4/24/2013 15:12'!
countAt: value
	"Even if we are not strictly 'counting'..."

	| n floatIndex i1 i2 |
	n _ tallies size.

	floatIndex _ (value * n) + 0.5.
	i1 _ floatIndex floor.
	i2 _ i1 + 1.

	i1 < 1 ifTrue: [
		^ tallies at: i2 ].

	i2 > n ifTrue: [
		^ tallies at: i1 ].

	^((tallies at: i1) * ( i2 - floatIndex)) + 
		((tallies at: i2) * (floatIndex - i1))! !

!ImageHistogram3 methodsFor: 'initialization' stamp: 'jmv 4/24/2013 12:11'!
initialize: anInteger

	tallies _ FloatArray new: anInteger! !

!ImageHistogram3 methodsFor: 'aux' stamp: 'jmv 4/24/2013 12:11'!
plot
	| rect |
	rect _ 10@10 extent: 768@400.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	FFT new plot: tallies in: rect color: Color black min: 0.0 max: tallies max! !

!ImageHistogram3 methodsFor: 'aux' stamp: 'jmv 4/24/2013 12:11'!
plot2
	"Just in case there is some difference with #plot"
	| rect |
	rect _ 10@10 extent: 768@400.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	FFT new
		plot: ((0.0 to: 1.0 count: rect width) collect: [ :v | self countAt: v ])
		in: rect
		color: Color red
		min: 0.0
		max: tallies max! !

!ImageHistogram3 methodsFor: 'aux' stamp: 'jmv 4/24/2013 12:11'!
plotIn: rect color: aColor min: min max: max
	FFT new plot: tallies in: rect color: aColor min: min max: max! !

!ImageHistogram3 methodsFor: 'aux' stamp: 'jmv 8/27/2014 15:02'!
plotLogIn: rect color: aColor min: min max: max
	FFT new plot: tallies log in: rect color: aColor min: min max: max! !

!ImageHistogram3 methodsFor: 'private' stamp: 'jmv 4/24/2013 14:20'!
tallies
	^tallies! !

!ImageHistogram3 class methodsFor: 'instance creation' stamp: 'jmv 4/24/2013 12:11'!
new: anInteger

	^self new initialize: anInteger! !

!FFT methodsFor: 'initialization' stamp: 'jmv 8/10/2010 10:51'!
basicRealData: real
	"Avoids conversion to FloatArray.
	This disables the fast plugin, but allows for 64 bit Double precision:"
	self
		basicRealData: real
		imagData: (real collect: [:i | 0.0])  "imaginary component all zero"! !

!FFT methodsFor: 'initialization' stamp: 'jmv 8/10/2010 10:59'!
basicRealData: real imagData: imag
	"Avoids conversion to FloatArray.
	If data is not in FloatArrays,  the fast plugin is disabled, but this allows for 64 bit Double precision:
	In any case, make the sinTable match the data, to allow either the plugin or the full precision"
	(real is: #FloatArray)
		ifTrue: [
			(sinTable is: #FloatArray)
				ifFalse: [ sinTable _ sinTable asFloatArray ]]
		ifFalse: [
			(sinTable is: #FloatArray)
				ifTrue: [ sinTable _ (0 to: n/4) collect: [:i | (i asFloat / (n//4) * Float pi / 2.0) sin ]]].
	realData _ real.
	imagData _ imag! !

!FFT methodsFor: 'testing' stamp: 'jm 8/1/1998 13:08'!
imagData

	^ imagData
! !

!FFT methodsFor: 'bulk processing' stamp: 'jm 9/8/1999 17:10'!
initializeHammingWindow: alpha
	"Initialize the windowing function to the generalized Hamming window. See F. Richard Moore, Elements of Computer Music, p. 100. An alpha of 0.54 gives the Hamming window, 0.5 gives the hanning window."

	| v midPoint |
	window _ FloatArray new: n.
	midPoint _ (n + 1) / 2.0.
	1 to: n do: [:i |
		v _ alpha + ((1.0 - alpha) * (2.0 * Float pi * ((i - midPoint) / n)) cos).
		window at: i put: v].

! !

!FFT methodsFor: 'bulk processing' stamp: 'jm 9/8/1999 16:42'!
initializeTriangularWindow
	"Initialize the windowing function to the triangular, or Parzen, window. See F. Richard Moore, Elements of Computer Music, p. 100."

	| v |
	window _ FloatArray new: n.
	0 to: (n // 2) - 1 do: [:i |
		v _ i / ((n // 2) - 1).
		window at: (i + 1) put: v.
		window at: (n - i) put: v].
! !

!FFT methodsFor: 'initialization' stamp: 'jm 8/25/1999 21:59'!
n

	^ n
! !

!FFT methodsFor: 'initialization' stamp: 'jmv 12/4/2012 23:41'!
nu: order
	"Initialize variables and tables for transforming 2^nu points"
	|  j perms k |
	nu _ order.
	n _ 2 bitShift: nu-1.

	"Initialize permutation table (bit-reversed indices)"
	j _ 0.
	perms _ WriteStream on: (Array new: n).
	0 to: n-2 do:
		[:i |
		i < j ifTrue: [perms nextPut: i+1; nextPut: j+1].
		k _ n // 2.
		[k <= j] whileTrue: [j _ j-k.  k _ k//2].
		j _ j + k].
	permTable _ perms contents asWordArray.

	"Initialize sin table 0..pi/2 in n/4 steps."
	sinTable _ (0 to: n/4) collect: [:i | (i asFloat / (n//4) * Float pi / 2.0) sin].
	
	sinTable _ sinTable asFloatArray.
	realData _ FloatArray new: n.
	imagData _ FloatArray new: n.

	self initializeHammingWindow: 0.54.  "0.54 for Hamming, 0.5 for hanning"! !

!FFT methodsFor: 'transforming' stamp: 'di 6/17/97 07:47'!
permuteData
	| i end a b |
	i _ 1.
	end _ permTable size.
	[i <= end] whileTrue:
		[a _ permTable at: i.
		b _ permTable at: i+1.
		realData swap: a with: b.
		imagData swap: a with: b.
		i _ i + 2]! !

!FFT methodsFor: 'testing' stamp: 'jmv 8/15/2012 15:35'!
plot: samples in: rect
	"Throw-away code just to check out a couple of examples"
	| min max x dx pen y |
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	min _ 1.0e30.  max _ -1.0e30.
	samples do: [ :v |
		min _ min min: v.
		max _ max max: v].
	pen _ Pen new.
	pen up.
	y _ (max/(max-min) * rect height + rect top).
	pen goto: rect left@ y.
	pen down.
	pen goto: rect right@ y.
	pen up.
	x _ rect left.
	dx _ rect width asFloat / (samples size-1).
	samples do: [ :v |
		y _ (max-v) / (max-min) * rect height asFloat.
		pen goto: x asInteger @ (rect top + y asInteger).
		pen down.
		x _ x + dx].
	max printString displayOn: Display at: (x+2) @ (rect top-9).
	min printString displayOn: Display at: (x+2) @ (rect bottom - 9)! !

!FFT methodsFor: 'testing' stamp: 'jmv 8/27/2014 15:07'!
plot: samples in: rect color: aColor min: min max: max
	"Throw-away code just to check out a couple of examples"
	| x dx pen y |
	pen _ Pen new.
	pen color: aColor.
	pen up.
	x _ rect left.
	dx _ rect width asFloat / (samples size-1).
	samples do: [ :v |
		y _ (max-v) / (max-min) * rect height asFloat.
		y _ y min: rect height.
		y _ y max: 0.
		pen goto: x asInteger @ (rect top + y asInteger).
		pen down.
		x _ x + dx].
	max printString displayOn: Display at: (x+2) @ (rect top-9).
	min printString displayOn: Display at: (x+2) @ (rect bottom - 9)! !

!FFT methodsFor: 'plugin-testing' stamp: 'jmv 3/13/2012 12:34'!
pluginTest  
	"
	Display restoreAfter: [(FFT new nu: 15) pluginTest].
	"
	"Test on an array of 32768 samples"
	"Initialize to pure (co)Sine Wave, plot, transform, plot, invert and plot again"
	self realData: ((1 to: n) collect: [:i | (Float pi * (i-1) / (n/8)) cos]).
	self plot: realData in: (100@20 extent: 256@60).
	Transcript newLine; print: (Time millisecondsToRun:[self pluginTransformData: true]); endEntry.
	self plot: realData in: (100@100 extent: 256@60).
	self plot: imagData in: (100@180 extent: 256@60).
	Transcript newLine; print: (Time millisecondsToRun:[self pluginTransformData: false]); endEntry.
	self plot: realData in: (100@260 extent: 256@60)! !

!FFT methodsFor: 'plugin-testing' stamp: 'ar 2/13/2001 21:10'!
pluginTransformData: forward
	"Plugin testing -- if the primitive is not implemented 
	or cannot be found run the simulation. See also: FFTPlugin"
	<primitive: 'primitiveFFTTransformData' module: 'FFTPlugin'>
	^(Smalltalk at: #FFTPlugin ifAbsent:[^self primitiveFailed])
		doPrimitive: 'primitiveFFTTransformData'.! !

!FFT methodsFor: 'testing' stamp: 'jm 8/1/1998 13:08'!
realData

	^ realData
! !

!FFT methodsFor: 'initialization' stamp: 'jmv 8/10/2010 10:49'!
realData: real
	"By defaults use FloatArrays to allow for the fast primitive.
	This means precision is limeted to 32bit Float.
	For 64 bit Double precision, use #basicRealData:"
	self basicRealData: real asFloatArray! !

!FFT methodsFor: 'initialization' stamp: 'jmv 8/10/2010 10:49'!
realData: real imagData: imag
	"By defaults use FloatArrays to allow for the fast primitive.
	This means precision is limeted to 32bit Float.
	For 64 bit Double precision, use #basicRealData:imagData:"
	self basicRealData: real asFloatArray imagData: imag asFloatArray! !

!FFT methodsFor: 'testing' stamp: 'jm 8/16/1998 17:36'!
samplesPerCycleForIndex: i
	"Answer the number of samples per cycle corresponding to a power peak at the given index. Answer zero if i = 1, since an index of 1 corresponds to the D.C. component."

	| windowSize |
	windowSize _ 2 raisedTo: nu.
	(i < 1 or: [i > (windowSize // 2)]) ifTrue: [^ self error: 'index is out of range'].
	i = 1 ifTrue: [^ 0].  "the D.C. component"
	^ windowSize asFloat / (i - 1)
! !

!FFT methodsFor: 'transforming' stamp: 'di 6/17/97 07:47'!
scaleData
	"Scale all elements by 1/n when doing inverse"
	| realN |
	realN _ n asFloat.
	1 to: n do:
		[:i |
		realData at: i put: (realData at: i) / realN.
		imagData at: i put: (imagData at: i) / realN]! !

!FFT methodsFor: 'bulk processing' stamp: 'jmv 8/10/2010 10:42'!
setSize: anIntegerPowerOfTwo
	"Initialize variables and tables for performing an FFT on the given number of samples. The number of samples must be an integral power of two (e.g. 1024). Prepare data for use with the fast primitive."

	self nu: (anIntegerPowerOfTwo log: 2) asInteger.
	n = anIntegerPowerOfTwo ifFalse: [self error: 'size must be a power of two']
! !

!FFT methodsFor: 'testing' stamp: 'di 6/17/97 07:47'!
test  "Display restoreAfter: [(FFT new nu: 8) test].  --  Test on an array of 256 samples"
	"Initialize to pure (co)Sine Wave, plot, transform, plot, invert and plot again"
	self realData: ((1 to: n) collect: [:i | (Float pi * (i-1) / (n/8)) cos]).
	self plot: realData in: (100@20 extent: 256@60).
	self transformForward: true.
	self plot: realData in: (100@100 extent: 256@60).
	self plot: imagData in: (100@180 extent: 256@60).
	self transformForward: false.
	self plot: realData in: (100@260 extent: 256@60)! !

!FFT methodsFor: 'testing' stamp: 'jmv 3/13/2012 12:34'!
testFullPrecision
	 "
	Display restoreAfter: [(FFT new nu: 15) testFullPrecision].
	  --  Test on an array of 32768 samples"
	"Initialize to pure (co)Sine Wave, plot, transform, plot, invert and plot again.
	Allow for full 64bit Double precision, do not use the fast plugin."
	self basicRealData: ((1 to: n) collect: [:i | (Float pi * (i-1) / (n/8)) cos]).
	self plot: realData in: (100@20 extent: 256@60).
	Transcript newLine; print: (Time millisecondsToRun: [ self transformForward: true ]); endEntry.
	self plot: realData in: (100@100 extent: 256@60).
	self plot: imagData in: (100@180 extent: 256@60).
	Transcript newLine; print: (Time millisecondsToRun:[self transformForward: false]); endEntry.
	self plot: realData in: (100@260 extent: 256@60)! !

!FFT methodsFor: 'bulk processing' stamp: 'jmv 8/10/2010 11:02'!
transformDataFrom: anIndexableCollection startingAt: index
	"Forward transform a block of real data taken from from the given indexable collection starting at the given index. Answer a block of values representing the normalized magnitudes of the frequency components."

	| j real imag out |
	j _ 0.
	index to: index + n - 1 do: [:i |
		realData at: (j _ j + 1) put: (anIndexableCollection at: i)].
	realData *= window.
	imagData _ FloatArray new: n.
	self transformForward: true.

	"compute the magnitudes of the complex results"
	"note: the results are in bottom half; the upper half is just its mirror image"
	real _ realData copyFrom: 1 to: (n / 2).
	imag _ imagData copyFrom: 1 to: (n / 2).
	out _ (real * real) + (imag * imag).
	1 to: out size do: [:i | out at: i put: (out at: i) sqrt].
	^ out
! !

!FFT methodsFor: 'transforming' stamp: 'jmv 8/10/2010 11:03'!
transformForward: forward
	| lev lev1 ip theta realU imagU realT imagT i |

	"Use the primitive if available and if data is in the correct format (FloatArrays and a WordArray for permTable)"
	<primitive: 'primitiveFFTTransformData' module: 'FFTPlugin'>

	self permuteData.
	1 to: nu do:
		[:level |
		lev _ 1 bitShift: level.
		lev1 _ lev // 2.
		1 to: lev1 do:
			[:j |
			theta _ j-1 * (n // lev).   "pi * (j-1) / lev1 mapped onto 0..n/2"
			theta < (n//4)  "Compute U, the complex multiplier for each level"
				ifTrue:
					[realU _ sinTable at: sinTable size - theta.
					imagU _ sinTable at: theta + 1]
				ifFalse:
					[realU _ (sinTable at: theta - (n//4) + 1) negated.
					imagU _ sinTable at: (n//2) - theta + 1].
			forward ifFalse: [imagU _ imagU negated].
"
			Here is the inner loop...
			j to: n by: lev do:
				[:i |   hand-transformed to whileTrue...
"
			i _ j.
			[i <= n] whileTrue:
				[ip _ i + lev1.
				realT _ ((realData at: ip) * realU) - ((imagData at: ip) * imagU).
				imagT _ ((realData at: ip) * imagU) + ((imagData at: ip) * realU).
				realData at: ip put: (realData at: i) - realT.
				imagData at: ip put: (imagData at: i) - imagT.
				realData at: i put: (realData at: i) + realT.
				imagData at: i put: (imagData at: i) + imagT.
				i _ i + lev]]].
	forward ifFalse: [self scaleData]  "Reverse transform must scale to be an inverse"! !

!FFT methodsFor: 'testing' stamp: 'jmv 8/28/2014 13:06'!
verticalLine: xx range: s in: rect color: aColor
	"Throw-away code just to check out a couple of examples"
	| x dx pen |
	xx <= s ifTrue: [
		pen _ Pen new.
		pen color: aColor.
		pen up.
		dx _ rect width asFloat / (s-1).
		x _ (dx * xx + rect left) asInteger.
		pen goto: x @ rect top.
		pen down.
		pen goto: x @ rect bottom ]! !

!FFT class methodsFor: 'instance creation' stamp: 'jm 8/25/1999 12:49'!
new: anIntegerPowerOfTwo
	"Answer a new FFT instance for transforming data packets of the given size."

	^ self new setSize: anIntegerPowerOfTwo
! !

!FFT2D methodsFor: 'outputs' stamp: 'jmv 9/17/2007 12:49'!
imagData
	^ imagMatrix! !

!FFT2D methodsFor: 'outputs' stamp: 'jmv 9/17/2007 12:49'!
realData
	^ realMatrix! !

!FFT2D methodsFor: 'inputs' stamp: 'jmv 9/17/2007 09:55'!
realData: real
	realMatrix _ real.
	imagMatrix _ nil! !

!FFT2D methodsFor: 'inputs' stamp: 'jmv 9/17/2007 09:55'!
realData: real imagData: imag
	realMatrix _ real.
	imagMatrix _ imag! !

!FFT2D methodsFor: 'computing' stamp: 'jmv 4/27/2015 10:39'!
transformColumns: forward
	| transform height realColumn imagColumn |
	height _ realMatrix height.
	transform _ FFT new: height.
	realColumn _ FloatMatrix newColumnVectorSize: height.
	imagColumn _ FloatMatrix newColumnVectorSize: height.
	1 to: realMatrix width do: [ :j |
		realMatrix j: j copyTo: realColumn.
		imagMatrix ifNotNil: [
			imagMatrix j: j copyTo: imagColumn ].
		transform realData: realColumn elements imagData: imagColumn elements.
		transform transformForward: forward.
		realMatrix j: j put: realColumn.
		imagMatrix ifNotNil: [
			imagMatrix j: j put: imagColumn ]]! !

!FFT2D methodsFor: 'computing' stamp: 'jmv 9/17/2007 09:56'!
transformForward: forward 
	self transformRows: forward;
		 transformColumns: forward! !

!FFT2D methodsFor: 'computing' stamp: 'jmv 4/27/2015 10:39'!
transformRows: forward
	| transform width realRow imagRow |
	width _ realMatrix width.
	transform _ FFT new: width.
	realRow _ FloatMatrix newRowVectorSize: width.
	imagRow _ FloatMatrix newRowVectorSize: width.
	1 to: realMatrix height do: [ :i |
		realMatrix i: i copyTo: realRow.
		imagMatrix ifNotNil: [
			imagMatrix i: i copyTo: imagRow ].
		transform realData: realRow elements imagData: imagRow elements.
		transform transformForward: forward.
		realMatrix i: i put: realRow.
		imagMatrix ifNotNil: [
			imagMatrix i: i put: imagRow ]]! !

!FWT methodsFor: 'access' stamp: 'di 10/31/1998 12:19'!
coeffs
	"Return all coefficients neede to reconstruct the original samples"
	| header csize strm |
	header _ Array with: nSamples with: nLevels with: alpha with: beta.
	csize _ header size.
	1 to: nLevels do: [:i | csize _ csize + (transform at: i*2) size].
	csize _ csize + (transform at: nLevels*2-1) size.
	coeffs _ Array new: csize.
	strm _ WriteStream on: coeffs.
	strm nextPutAll: header.
	1 to: nLevels do: [:i | strm nextPutAll: (transform at: i*2)].
	strm nextPutAll: (transform at: nLevels*2-1).
	^ coeffs! !

!FWT methodsFor: 'access' stamp: 'di 10/31/1998 12:23'!
coeffs: coeffArray
	"Initialize this instance from the given coeff array (including header)."
	| header strm |
	strm _ ReadStream on: coeffArray.
	header _ strm next: 4.
	self nSamples: header first nLevels: header second.
	self setAlpha: header third beta: header fourth.
	1 to: nLevels do: [:i | transform at: i*2 put: (strm next: (transform at: i*2) size)].
	transform at: nLevels*2-1 put: (strm next: (transform at: nLevels*2-1) size).
	strm atEnd ifFalse: [self error: 'Data size error'].
! !

!FWT methodsFor: 'computation' stamp: 'di 10/31/1998 09:20'!
convolveAndDec: inData dataLen: inLen filter: filter out: outData
	"convolve the input sequence with the filter and decimate by two"
	| filtLen offset outi dotp |
	filtLen _ filter size.
	outi _ 1.
	1 to: inLen+9 by: 2 do:
		[:i | 
		i < filtLen
		ifTrue:
			[dotp _ self dotpData: inData endIndex: i filter: filter
						start: 1 stop: i inc: 1]
		ifFalse:
			[i > (inLen+5)
			ifTrue:
				[offset _ i - (inLen+5).
				dotp _ self dotpData: inData endIndex: inLen+5 filter: filter
						start: 1+offset stop: filtLen inc: 1]
			ifFalse:
				[dotp _ self dotpData: inData endIndex: i filter: filter
						start: 1 stop: filtLen inc: 1]].
		outData at: outi put: dotp.
		outi _ outi + 1]! !

!FWT methodsFor: 'computation' stamp: 'ls 10/10/1999 13:13'!
convolveAndInt: inData dataLen: inLen filter: filter sumOutput:
sumOutput into: outData
	"insert zeros between each element of the input sequence and
	   convolve with the filter to interpolate the data"
	| outi filtLen oddTerm evenTerm j |
	outi _ 1.
	filtLen _ filter size.

	"every other dot product interpolates the data"
	filtLen // 2 to: inLen + filtLen - 2 do:
		[:i |
		oddTerm _ self dotpData: inData endIndex: i filter: filter
									start: 2 stop: filter size inc: 2.
		evenTerm _ self dotpData: inData endIndex: i+1 filter: filter
									start: 1 stop: filter size inc: 2.
		sumOutput
			ifTrue:
				["summation with previous convolution if true"
				outData at: outi put: (outData at: outi) + oddTerm.
				outData at: outi+1 put: (outData at: outi+1) + evenTerm]
			ifFalse:
				["first convolution of pair if false"
				outData at: outi put: oddTerm.
				outData at: outi+1 put: evenTerm].
		outi _ outi + 2].

	"Ought to be able to fit this last term into the above loop."
	j _ inLen + filtLen - 1.
	oddTerm _ self dotpData: inData endIndex: j filter: filter
									start: 2 stop: filter size inc: 2.
	sumOutput
		ifTrue: [outData at: outi put: (outData at: outi) + oddTerm]
		ifFalse: [outData at: outi put: oddTerm].
! !

!FWT methodsFor: 'testing' stamp: 'di 10/31/1998 12:25'!
doWaveDemo  "FWT new doWaveDemo"
	"Printing the above should yield a small number -- I get 1.1e-32"
	| originalData |
	self nSamples: 312 nLevels: 3.
	self setAlpha: 0.0 beta: 0.0.

	"Install a sine wave as sample data"
	self samples: ((1 to: nSamples) collect: [:i | ((i-1) * 0.02 * Float pi) sin]).
	originalData _ samples copy.
	FFT new plot: (samples copyFrom: 1 to: nSamples) in: (0@0 extent: nSamples@100).

	"Transform forward and plot the decomposition"
	self transformForward: true.
	transform withIndexDo:
		[:w :i |
		FFT new plot: (w copyFrom: 1 to: w size-5)
			in: (i-1\\2*320@(i+1//2*130) extent: (w size-5)@100)].

	"Test copy out and read in the transform coefficients"
	self coeffs: self coeffs.

	"Ttransform back, plot the reconstruction, and return the error figure"
	self transformForward: false.
	FFT new plot: (samples copyFrom: 1 to: nSamples) in: (320@0 extent: nSamples@100).
	^ self meanSquareError: originalData! !

!FWT methodsFor: 'computation' stamp: 'di 10/31/1998 12:55'!
dotpData: data endIndex: endIndex filter: filter start: start stop: stop inc: inc
	| sum i j |
	sum _ 0.0.
	j _ endIndex.
	i _ start.
	[i <= stop] whileTrue:
		[sum _ sum + ((data at: j) * (filter at: i)).
		i _ i + inc.
		j _ j - 1].
	^ sum! !

!FWT methodsFor: 'testing' stamp: 'di 10/30/1998 15:58'!
meanSquareError: otherData
	"Return the mean-square error between the current sample array and
	some other data, presumably to evaluate a compression scheme."
	| topSum bottomSum pointDiff |
	topSum _ bottomSum _ 0.0.
	1 to: nSamples do:
		[:i |  pointDiff _ (samples at: i) - (otherData at: i).
		topSum _ topSum + (pointDiff * pointDiff).
		bottomSum _ bottomSum + ((otherData at: i) * (otherData at: i))].
	^ topSum / bottomSum! !

!FWT methodsFor: 'initialization' stamp: 'di 10/31/1998 12:23'!
nSamples: n nLevels: nLevs
	"Initialize a wavelet transform."
	"Note the sample array size must be N + 5, where N is a multiple of 2^nLevels"
	| dyadSize |
	(n // (1 bitShift: nLevs)) > 0 ifFalse: [self error: 'Data size error'].
	(n \\ (1 bitShift: nLevs)) = 0 ifFalse: [self error: 'Data size error'].
	nSamples _ n.
	samples _ Array new: n + 5.
	nLevels _ nLevs.
	transform _ Array new: nLevels*2.  "Transformed data is stored as a tree of coeffs"
	dyadSize _ nSamples.
	1 to: nLevels do:
		[:i |  dyadSize _ dyadSize // 2.
		transform at: 2*i-1 put: (Array new: dyadSize + 5).
		transform at: 2*i put: (Array new: dyadSize + 5)]! !

!FWT methodsFor: 'access' stamp: 'di 10/31/1998 12:26'!
samples
	^ samples copyFrom: 1 to: nSamples! !

!FWT methodsFor: 'access' stamp: 'di 10/31/1998 12:25'!
samples: anArray
	1 to: anArray size do:
		[:i | samples at: i put: (anArray at: i)].
	nSamples+1 to: nSamples+5 do:
		[:i | samples at: i put: 0.0]! !

!FWT methodsFor: 'initialization' stamp: 'di 10/30/1998 10:59'!
setAlpha: alph beta: bet
	"Set alpha and beta, compute wavelet coeefs, and derive hFilter and lFilter"
	| tcosa tcosb tsina tsinb |
	alpha _ alph.
	beta _ bet.

	"WaveletCoeffs..."
	"precalculate cosine of alpha and sine of beta"
	tcosa _ alpha cos.
	tcosb _ beta cos.
	tsina _ alpha sin.
	tsinb _ beta sin.
	coeffs _ Array new: 6.
	
	"calculate first two wavelet coefficients a _ a(-2) and b _ a(-1)"
	coeffs at: 1 put: ((1.0 + tcosa + tsina) * (1.0 - tcosb - tsinb)
					+ (2.0 * tsinb * tcosa)) / 4.0.
	coeffs at: 2 put: ((1.0 - tcosa + tsina) * (1.0 + tcosb - tsinb)
					- (2.0 * tsinb * tcosa)) / 4.0.

	"precalculate cosine and sine of alpha minus beta"
	tcosa _ (alpha - beta) cos.
	tsina _ (alpha - beta) sin.

	"calculate last four wavelet coefficients c _ a(0), d _ a(1), e _ a(2), and f _ a(3)"
	coeffs at: 3 put: (1.0 + tcosa + tsina) / 2.0.
	coeffs at: 4 put: (1.0 + tcosa - tsina) / 2.0.
	coeffs at: 5 put: 1.0 - (coeffs at: 1) - (coeffs at: 3).
	coeffs at: 6 put: 1.0 - (coeffs at: 2) - (coeffs at: 4).

	"MakeFiltersFromCoeffs..."
	"Select the non-zero wavelet coefficients"
	coeffs _ coeffs copyFrom: (coeffs findFirst: [:c | c abs > 1.0e-14])
						to: (coeffs findLast: [:c | c abs > 1.0e-14]).

	"Form the low pass and high pass filters for decomposition"
	hTilde _ coeffs reversed collect: [:c | c / 2.0].
	gTilde _ coeffs collect: [:c | c / 2.0].
	1 to: gTilde size by: 2 do:
		[:i | gTilde at: i put: (gTilde at: i) negated].

	"Form the low pass and high pass filters for reconstruction"
	h _ coeffs copy.
	g _ coeffs reversed.
	2 to: g size by: 2 do:
		[:i | g at: i put: (g at: i) negated]
! !

!FWT methodsFor: 'computation' stamp: 'di 10/30/1998 15:53'!
transformForward: forward
	| inData inLen outData |
	forward
	ifTrue:
		["first InData is input signal, following are intermediate approx coefficients"
		inData _ samples.  inLen _ nSamples.
		1 to: nLevels do:
			[:i |
			self convolveAndDec: inData dataLen: inLen
					filter: hTilde out: (transform at: 2*i-1).
			self convolveAndDec: inData dataLen: inLen
					filter: gTilde out: (transform at: 2*i).
			inData _ transform at: 2*i-1.  inLen _ inLen // 2]]
	ifFalse:
		[inLen _ nSamples >> nLevels.
		"all but last outData are next higher intermediate approximations,
		last is final reconstruction of samples"
		nLevels to: 1 by: -1 do:
			[:i |
			outData _ i = 1 ifTrue: [samples]
						ifFalse: [transform at: 2*(i-1)-1].
			self convolveAndInt: (transform at: 2*i-1) dataLen: inLen
					filter: h sumOutput: false into: outData.
			self convolveAndInt: (transform at: 2*i) dataLen: inLen
					filter: g sumOutput: true into: outData.
			inLen _ inLen * 2]]
! !

!FWT methodsFor: 'testing' stamp: 'jmv 1/14/2013 21:12'!
viewPhiAndPsi  "(FWT new nSamples: 256 nLevels: 6) viewPhiAndPsi"
	"View the scaling function and mother wavelets for this transform"
	| p |
	Display fillWhite: (0@0 extent: 300@300).
	Display border: (0@0 extent: 300@300) width: 2.
	[Sensor isAnyButtonPressed] whileFalse:
		["Move mouse around in the outer rectangle to explore"
		p _ Sensor mousePoint min: 300@300.
		self setAlpha: (p x - 150) / 150.0 * Float pi
				beta: (p y - 150) / 150.0 * Float pi.
		'alpha=', (alpha roundTo: 0.01) printString, '   ',
			'beta=', (beta roundTo: 0.01) printString, '    ' displayAt: 50@5.
		transform do: [:w | w atAllPut: 0.0].
		(transform at: transform size - 1) at: (nSamples>>nLevels) put: 1.0.
		self transformForward: false.
		FFT new plot: (samples copyFrom: 1 to: nSamples) in: (20@30 extent: nSamples@100).

		transform do: [:w | w atAllPut: 0.0].
		(transform at: transform size) at: (nSamples>>nLevels) put: 1.0.
		self transformForward: false.
		FFT new plot: (samples copyFrom: 1 to: nSamples) in: (20@170 extent: nSamples@100)].
	Sensor waitNoButton! !
