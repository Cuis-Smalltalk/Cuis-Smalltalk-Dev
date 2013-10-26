'From Cuis 4.0 of 21 April 2012 [latest update: #1295] on 6 June 2012 at 10:42:56 pm'!

!ThemesTest methodsFor: 'as yet unclassified' stamp: 'jmv 6/6/2012 22:37'!
setUp
	"Create an anonymous subclass of Theme, override #shout.
	We do this because the ouput of #generateShoutConfig, the
	method under test, is dependent on the color data answered 
	by #shout, and we don't want the test to fail just because 
	someone changed the colors on the default theme (Theme.)"

	| metaclass |

	metaclass := Metaclass new
		superclass: Theme class;
		methodDictionary: MethodDictionary new;
		setFormat: Theme class format;
		yourself.

	theme := metaclass new
		superclass: Theme;
		methodDictionary: MethodDictionary new;
		setFormat: Theme format;
		yourself.
		
	theme compile:
'	shout
	"Color symbols as an association list."
	
	^ {
		#defaults 				-> #black.
		#undefined 			-> #green.
		#methodTags 			-> #(green muchDarker).
		#pseudoVariables 		-> #(red muchDarker).
		#messages 			-> #(blue muchDarker).
		#arguments 			-> #(cyan muchDarker).
		#instVar 				-> #(magenta muchDarker).
		#incompleteMessages -> #(gray veryMuchDarker).
		#blockLevelFour 		-> #(green darker).
		#blockLevelFive 		-> #(orange darker).
		#blockLevelSix 		-> #(magenta darker).
		#blockLevelSeven 		-> #blue.
		#tempBar 				-> #gray.
		#tempVars 			-> #(gray muchDarker).
	}
'! !

!ThemesTest methodsFor: 'as yet unclassified' stamp: 'jmv 6/6/2012 22:42'!
shoutArray
	^  #(#(#invalid #green) #(#excessCode #green) #(#'$' #green) #(#undefinedKeyword #green) #(#undefinedBinary #green) #(#undefinedUnary #green) #(#character #(#red #muchDarker)) #(#integer #(#red #muchDarker)) #(#number #(#red #muchDarker)) #(#- #(#red #muchDarker)) #(#blockStart3 #(#red #muchDarker)) #(#blockEnd3 #(#red #muchDarker)) #(#leftParenthesis3 #(#red #muchDarker)) #(#rightParenthesis3 #(#red #muchDarker)) #(#default #black) #(#arrayStart #black) #(#arrayEnd #black) #(#arrayStart1 #black) #(#arrayEnd1 #black) #(#leftBrace #black) #(#rightBrace #black) #(#cascadeSeparator #black) #(#statementSeparator #black) #(#externalCallType #black) #(#externalCallTypePointerIndicator #black) #(#rightParenthesis1 #black) #(#blockArgColon #black) #(#leftParenthesis #black) #(#rightParenthesis #black) #(#blockStart #black) #(#blockEnd #black) #(#self #(#red #muchDarker)) #(#super #(#red #muchDarker)) #(#true #(#red #muchDarker)) #(#false #(#red #muchDarker)) #(#nil #(#red #muchDarker)) #(#thisContext #(#red #muchDarker)) #(#return #(#red #muchDarker)) #(#blockStart4 #(#green #darker)) #(#blockEnd4 #(#green #darker)) #(#leftParenthesis4 #(#green #darker)) #(#rightParenthesis4 #(#green #darker)) #(#instVar #(#magenta #muchDarker)) #(#blockStart2 #(#magenta #muchDarker)) #(#blockEnd2 #(#magenta #muchDarker)) #(#leftParenthesis2 #(#magenta #muchDarker)) #(#rightParenthesis2 #(#magenta #muchDarker)) #(#keyword #(#blue #muchDarker)) #(#binary #(#blue #muchDarker)) #(#unary #(#blue #muchDarker)) #(#leftParenthesis5 #(#orange #darker)) #(#rightParenthesis5 #(#orange #darker)) #(#blockStart5 #(#orange #darker)) #(#blockEnd5 #(#orange #darker)) #(#leftParenthesis6 #(#magenta #darker)) #(#rightParenthesis6 #(#magenta #darker)) #(#blockStart6 #(#magenta #darker)) #(#blockEnd6 #(#magenta #darker)) #(#leftParenthesis7 #blue) #(#rightParenthesis7 #blue) #(#blockStart7 #blue) #(#blockEnd7 #blue) #(#methodTempBar #gray) #(#blockTempBar #gray) #(#blockArgsBar #gray) #(#primitive #(#green #muchDarker) #bold) #(#pragmaKeyword #(#green #muchDarker) #bold) #(#pragmaUnary #(#green #muchDarker) #bold) #(#pragmaBinary #(#green #muchDarker) #bold) #(#externalFunctionCallingConvention #(#green #muchDarker) #bold) #(#module #(#green #muchDarker) #bold) #(#primitiveOrExternalCallStart #black #bold) #(#primitiveOrExternalCallEnd #black #bold) #(#globalVar #black #bold) #(#workspaceVar #black #bold) #(#incompleteKeyword #(#gray #veryMuchDarker) #underlined) #(#incompleteBinary #(#gray #veryMuchDarker) #underlined) #(#incompleteUnary #(#gray #veryMuchDarker) #underlined) #(#patternArg #(#cyan #muchDarker) #italic) #(#methodArg #(#cyan #muchDarker) #italic) #(#blockPatternArg #(#cyan #muchDarker) #italic) #(#blockArg #(#cyan #muchDarker) #italic) #(#argument #(#cyan #muchDarker) #italic) #(#symbol #(#blue #muchDarker) #bold) #(#stringSymbol #(#blue #muchDarker) #bold) #(#literalArray #(#blue #muchDarker) #bold) #(#assignment nil #bold) #(#ansiAssignment nil #bold) #(#patternKeyword nil #bold) #(#patternBinary nil #bold) #(#patternUnary nil #bold) #(#tempVar #(#gray #muchDarker) #italic) #(#patternTempVar #(#gray #muchDarker) #italic) #(#poolConstant #(#gray #muchDarker) #italic) #(#blockTempVar #gray #italic) #(#blockPatternTempVar #gray #italic) #(#unfinishedString #green #normal) #(#undefinedIdentifier #green #bold) #(#unfinishedComment #(#red #muchDarker) #italic) #(#comment #(#green #muchDarker) #italic) #(#string #(#magenta #muchDarker) #normal) #(#literal nil #italic) #(#incompleteIdentifier #(#gray #muchDarker) #(#italic #underlined)) #(#classVar #(#gray #muchDarker) #bold))! !

