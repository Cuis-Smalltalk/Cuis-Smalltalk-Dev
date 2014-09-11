'From Cuis 4.2 of 25 July 2013 [latest update: #2090] on 8 September 2014 at 10:41:13.932702 pm'!

!WorldState methodsFor: 'update cycle' stamp: 'jmv 9/8/2014 22:33'!
displayWorldSafely: aWorld
	"Update this world's display and keep track of errors during draw methods."

	[aWorld displayWorld] ifError: [:err :rcvr |
		"Handle a drawing error"
		| errCtx errMorph |
		errCtx _ thisContext.
		[
			errCtx _ errCtx sender.
			"Search the sender chain to find the morph causing the problem"
			[errCtx notNil and: [ (errCtx receiver is: #Morph) not ]] 
				whileTrue:[errCtx _ errCtx sender].
			"If we're at the root of the context chain then we have a fatal drawing problem"
			errCtx ifNil:[^self handleFatalDrawingError: err].
			errMorph _ errCtx receiver.
			"If the morph causing the problem has already the #drawError flag set,
			then search for the next morph above in the caller chain."
			errMorph isKnownFailing
		] whileTrue.
		errMorph drawingFails.
		self clearCanvas.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	].! !
