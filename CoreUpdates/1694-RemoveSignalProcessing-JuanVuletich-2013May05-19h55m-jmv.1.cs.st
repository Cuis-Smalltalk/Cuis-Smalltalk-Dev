'From Cuis 4.1 of 12 December 2012 [latest update: #1693] on 5 May 2013 at 7:56:29.9287 pm'!
!methodRemoval: SampledSound #sonogramMorph:from:to:nPoints:!
SampledSound removeSelector: #sonogramMorph:from:to:nPoints:!
!methodRemoval: LoopedSampledSound #copyDownSampledLowPassFiltering:!
LoopedSampledSound removeSelector: #copyDownSampledLowPassFiltering:!
!methodRemoval: LoopedSampledSound #downSampleLowPassFiltering:!
LoopedSampledSound removeSelector: #downSampleLowPassFiltering:!
!methodRemoval: LoopedSampledSound #fftAt:!
LoopedSampledSound removeSelector: #fftAt:!
!methodRemoval: LoopedSampledSound #fftWindowSize:startingAt:!
LoopedSampledSound removeSelector: #fftWindowSize:startingAt:!
!methodRemoval: LoopedSampledSound #highestSignificantFrequencyAt:!
LoopedSampledSound removeSelector: #highestSignificantFrequencyAt:!
!methodRemoval: LoopedSampledSound #normalizedResultsFromFFT:!
LoopedSampledSound removeSelector: #normalizedResultsFromFFT:!

!LoopedSampledSound reorganize!
('initialization' addReleaseEnvelope computeSampleCountForRelease fromAIFFFileNamed:mergeIfStereo: fromAIFFFileReader:mergeIfStereo: initialize samples:loopEnd:loopLength:pitch:samplingRate: setPitch:dur:loudness: unloopedSamples:pitch:samplingRate:)
('accessing' beUnlooped duration duration: firstSample firstSample: gain gain: isLooped isStereo leftSamples leftSamples: loopEnd loopLength originalSamplingRate perceivedPitch pitch pitch: rightSamples rightSamples: samples)
('sound generation' mixSampleCount:into:startingAt:leftVol:rightVol: reset samplesRemaining stopAfterMSecs:)
('other' findStartPointAfter: findStartPointForThreshold: indexOfFirstPointOverThreshold:)
('disk i/o' comeFullyUpOnReload: objectForDataStream:)
('file i/o' storeSampleCount:bigEndian:on:)
!

!methodRemoval: FloatMatrix #clearHighFftFrequencies!
FloatMatrix removeSelector: #clearHighFftFrequencies!
!methodRemoval: FloatMatrix #convolutionLinearWith:!
FloatMatrix removeSelector: #convolutionLinearWith:!
!methodRemoval: FloatMatrix #convolveWith:!
FloatMatrix removeSelector: #convolveWith:!
!methodRemoval: FloatMatrix #fftImaginary:forward:!
FloatMatrix removeSelector: #fftImaginary:forward:!
!methodRemoval: FloatMatrix #fftRealForward:!
FloatMatrix removeSelector: #fftRealForward:!
!methodRemoval: FloatMatrix #fillGaussian!
FloatMatrix removeSelector: #fillGaussian!
!methodRemoval: FloatMatrix #fillLowPassGaussian:!
FloatMatrix removeSelector: #fillLowPassGaussian:!
!methodRemoval: FloatMatrix #gaussianI:j:sigma:!
FloatMatrix removeSelector: #gaussianI:j:sigma:!
!methodRemoval: FloatMatrix #lowPassFilter!
FloatMatrix removeSelector: #lowPassFilter!
!methodRemoval: FloatMatrix #lowPassGaussian:i:j:!
FloatMatrix removeSelector: #lowPassGaussian:i:j:!
!methodRemoval: FloatMatrix #normalizeMax!
FloatMatrix removeSelector: #normalizeMax!
!methodRemoval: FloatMatrix #normalizeSum!
FloatMatrix removeSelector: #normalizeSum!
!methodRemoval: FloatMatrix #scatterPlotWith:size:!
FloatMatrix removeSelector: #scatterPlotWith:size:!
!methodRemoval: FloatMatrix #subsampled!
FloatMatrix removeSelector: #subsampled!
!methodRemoval: FloatMatrix #subsampled:!
FloatMatrix removeSelector: #subsampled:!
!methodRemoval: FloatMatrix #subsampled:extent:!
FloatMatrix removeSelector: #subsampled:extent:!

!FloatMatrix reorganize!
('accessing' bilinearInterpolationAtX:y: fillWithZeroes max min)
('initialization' initializeElements)
('aritmethic - complex' imaginary:elementDivideBy:imaginary: imaginary:elementDivideBy:imaginary:threshold: imaginary:elementMultiplyBy:imaginary: magnitudeWithImaginary:)
('misc' addNormalNoise:seed:)
('filling' fillWithIdentity)
('linear equation systems' linesLost)
('norm and condition number' conditionNumber norm norm2 norm2Squared)
('eigenvalues and eigenvectors' eigenvaluesAndEigenvectors eigenvaluesAndEigenvectorsByQR eigenvaluesByQR hessembergSuperior storeHouseholderTransformOn:column:forQR: storeQRDecompositionOfHessemberSuperiorOnQ:r:)
('operations' round)
('printing' printOn:)
('aux operations' preMultiplyByGivensRotationRowI:rowK:titaCosine:titaSine: rowsCloserToZero:)
('private' epsilon)
('testing' isFirstSubdiagonalZero)
!

!classRemoval: #FFT!
Smalltalk removeClassNamed: #FFT!
!classRemoval: #FFT2D!
Smalltalk removeClassNamed: #FFT2D!
!classRemoval: #FWT!
Smalltalk removeClassNamed: #FWT!
!classRemoval: #WaveletCodec!
Smalltalk removeClassNamed: #WaveletCodec!
