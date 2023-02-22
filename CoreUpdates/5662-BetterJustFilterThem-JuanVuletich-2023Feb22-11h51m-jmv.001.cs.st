'From Cuis 6.0 [latest update: #5661] on 22 February 2023 at 11:55:35 am'!

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 1/14/2023 20:52:28'!
processCommandLineArguments
	"
	Smalltalk processCommandLineArguments

	Note: By default, any exceptions thrown during the evaluation of arguments are simply ignored.
	To ease debugging, use the -e argument: Do Not Ignore Exceptions. 
	For example:
		squeak Cuis.image -e -s myScript.st
	If ``myScript.st`` contains an error, a Debugger will pop up.
	
	See comment at
		processCommandLineOption:optionArgument:raiseExceptions:
	See displayCommandLineUsageOn:
	"
	| optionString optionCharacter optionArgument optionIndex nextOptionIndex optionLastParameterIndex raiseExceptions |

	"Any command line argument that starts with $- is an Option.
	NonOption arguments are parameters for the preceding Option."
	raiseExceptions _ false.
	startUpScriptArguments _ #().
	optionIndex _ startUpArguments findFirst: [ :any | any first = $- ] startingAt: 1.
	[ optionIndex > 0 ] whileTrue: [
		"Find parameters for this option"
		nextOptionIndex _ startUpArguments findFirst: [ :any | any first = $- ] startingAt: optionIndex+1.
		optionLastParameterIndex _ nextOptionIndex = 0
			ifTrue: [startUpArguments size]
			ifFalse: [nextOptionIndex-1].
		optionString _ startUpArguments at: optionIndex.
		optionString size > 1 ifTrue: [
			optionCharacter _ optionString second.
			optionCharacter = $- "Including    --    as an option means 'ignore everything after here'"
				ifTrue: [^self].
			optionCharacter = $e
				ifTrue: [ raiseExceptions _ true ]
				ifFalse: [
					"Handle both    -rRequiredFeature2    and    -r RequiredFeature2"
					optionString size > 2
						ifTrue: [
							optionArgument _ optionString copyFrom: 3 to: optionString size.
							startUpScriptArguments _ startUpArguments copyFrom: optionIndex+1 to: optionLastParameterIndex ]
						ifFalse: [
							optionArgument _ optionLastParameterIndex > optionIndex ifTrue: [startUpArguments at: optionIndex+1].
							startUpScriptArguments _ startUpArguments copyFrom: optionIndex+2 to: optionLastParameterIndex ].
					self processCommandLineOption: optionCharacter optionArgument: optionArgument raiseExceptions: raiseExceptions.
					startUpScriptArguments _ #() ]
			].
		optionIndex _ nextOptionIndex ].! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 2/22/2023 11:54:51'!
readCommandLineArguments
	"
	Smalltalk readCommandLineArguments
	"
	| arg i |
	startUpArguments _ Array streamContents: [ :strm |
		i _ 2.
		[ i <= 1000 and: [ (arg _ self getSystemAttribute: i) notNil ] ] whileTrue: [
			"See https://lists.cuis.st/mailman/archives/cuis-dev/2023-February/006872.html"
			arg notEmpty ifTrue: [
				strm nextPut: arg ].
			i _ i + 1 ]].! !

