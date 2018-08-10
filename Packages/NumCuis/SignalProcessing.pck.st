'From Cuis 5.0 of 7 November 2016 [latest update: #3398] on 9 August 2018 at 11:34:45 am'!
'Description Image Processing and Signal Processing algorithms. FloatImage, Interpolation, Histograms, etc.'!
!provides: 'SignalProcessing' 1 103!
!requires: 'Pen' 1 0 nil!
!requires: 'LinearAlgebra' 1 nil nil!
!requires: 'Sound' 1 nil nil!
!requires: 'Collections-CompactArrays' 1 2 nil!
SystemOrganization addCategory: #SignalProcessing!
SystemOrganization addCategory: #'SignalProcessing-SoundSynthesis'!


!classDefinition: #WaveletCodec category: #'SignalProcessing-SoundSynthesis'!
SoundCodec subclass: #WaveletCodec
	instanceVariableNames: 'fwt samplesPerFrame nLevels alpha beta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing-SoundSynthesis'!
!classDefinition: 'WaveletCodec class' category: #'SignalProcessing-SoundSynthesis'!
WaveletCodec class
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

!classDefinition: #FWT category: #SignalProcessing!
Object subclass: #FWT
	instanceVariableNames: 'alpha beta coeffs h g hTilde gTilde samples nSamples nLevels transform'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SignalProcessing'!
!classDefinition: 'FWT class' category: #SignalProcessing!
FWT class
	instanceVariableNames: ''!


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

!FFT commentStamp: '<historical>' prior: 0!
This class implements the Fast Fourier Transform roughly as described on page 367
of "Theory and Application of Digital Signal Processing" by Rabiner and Gold.
Each instance caches tables used for transforming a given size (n = 2^nu samples) of data.

It would have been cleaner using complex numbers, but often the data is all real.!

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

!FFT methodsFor: 'testing' stamp: 'jm 8/1/1998 13:08'!
imagData

	^ imagData
! !

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

!FFT methodsFor: 'testing' stamp: 'jm 8/1/1998 13:08'!
realData

	^ realData
! !

!FFT methodsFor: 'testing' stamp: 'jm 8/16/1998 17:36'!
samplesPerCycleForIndex: i
	"Answer the number of samples per cycle corresponding to a power peak at the given index. Answer zero if i = 1, since an index of 1 corresponds to the D.C. component."

	| windowSize |
	windowSize _ 2 raisedTo: nu.
	(i < 1 or: [i > (windowSize // 2)]) ifTrue: [^ self error: 'index is out of range'].
	i = 1 ifTrue: [^ 0].  "the D.C. component"
	^ windowSize asFloat / (i - 1)
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

!FFT methodsFor: 'bulk processing' stamp: 'jmv 8/10/2010 10:42'!
setSize: anIntegerPowerOfTwo
	"Initialize variables and tables for performing an FFT on the given number of samples. The number of samples must be an integral power of two (e.g. 1024). Prepare data for use with the fast primitive."

	self nu: (anIntegerPowerOfTwo log: 2) asInteger.
	n = anIntegerPowerOfTwo ifFalse: [self error: 'size must be a power of two']
! !

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

!FFT methodsFor: 'transforming' stamp: 'di 6/17/97 07:47'!
scaleData
	"Scale all elements by 1/n when doing inverse"
	| realN |
	realN _ n asFloat.
	1 to: n do:
		[:i |
		realData at: i put: (realData at: i) / realN.
		imagData at: i put: (imagData at: i) / realN]! !

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

!FFT class methodsFor: 'instance creation' stamp: 'jm 8/25/1999 12:49'!
new: anIntegerPowerOfTwo
	"Answer a new FFT instance for transforming data packets of the given size."

	^ self new setSize: anIntegerPowerOfTwo
! !

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

!FWT methodsFor: 'access' stamp: 'di 10/31/1998 12:26'!
samples
	^ samples copyFrom: 1 to: nSamples! !

!FWT methodsFor: 'access' stamp: 'di 10/31/1998 12:25'!
samples: anArray
	1 to: anArray size do:
		[:i | samples at: i put: (anArray at: i)].
	nSamples+1 to: nSamples+5 do:
		[:i | samples at: i put: 0.0]! !

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
