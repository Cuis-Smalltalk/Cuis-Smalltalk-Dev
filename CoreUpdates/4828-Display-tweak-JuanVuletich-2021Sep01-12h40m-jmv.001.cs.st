'From Cuis 5.0 [latest update: #4827] on 1 September 2021 at 12:41:09 pm'!

!DisplayScreen methodsFor: 'private' stamp: 'jmv 9/1/2021 12:40:52'!
setExtent: aPoint depth: bitsPerPixel 
	"DisplayScreen startUp"
	"This method is critical.  If the setExtent fails, there will be no
	proper display on which to show the error condition..."

	| bitsPerPixelToUse |
	(depth = bitsPerPixel and: [aPoint = self extent and: [
					self supportsDisplayDepth: bitsPerPixel]]) ifFalse: [
		bits _ nil.  "Free up old bitmap in case space is low"
		bitsPerPixelToUse _ (self supportsDisplayDepth: bitsPerPixel)
			ifTrue: [ bitsPerPixel ]
			ifFalse: [
				(self supportsDisplayDepth: bitsPerPixel negated)
					ifTrue: [ bitsPerPixel negated ]
					ifFalse: [ self findAnyDisplayDepth ]].
		super setExtent: aPoint depth: bitsPerPixelToUse.
	].

	"Let the world know"
	self triggerEvent: #screenSizeChanged.! !

