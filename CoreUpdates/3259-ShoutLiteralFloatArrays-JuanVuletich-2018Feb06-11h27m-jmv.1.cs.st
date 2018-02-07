'From Cuis 5.0 of 7 November 2016 [latest update: #3257] on 6 February 2018 at 11:27:37 am'!

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 2/6/2018 11:27:31'!
parseByteArray
	"Literal ByteArray or literal FloatArray"
	[currentTokenFirst == $]] whileFalse: [
		currentTokenFirst isDigit | (currentTokenFirst = $-)
			ifTrue: [
				"do not parse the number, can be time consuming"
				self scanPast: #number]
			ifFalse: [
				self failWhen: currentTokenFirst == $. .
				self error]].
	self scanPast: #arrayEnd! !

