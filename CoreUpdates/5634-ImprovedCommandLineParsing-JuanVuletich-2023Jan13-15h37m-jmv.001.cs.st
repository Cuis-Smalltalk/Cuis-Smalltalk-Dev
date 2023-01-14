'From Cuis 6.0 [latest update: #5631] on 13 January 2023 at 4:08:18 pm'!

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 1/13/2023 15:49:48'!
displayCommandLineUsageOn: aStream
	"Display command line usage on standard out."

	[ aStream
		nextPutAll: 'Command line options:'; newLine;
		nextPutAll: '  -h Display this help message and quit'; newLine;
		nextPutAll: '  -v Display the system information, containing the version and quit'; newLine;
		nextPutAll: '  -q Just quit'; newLine;
		nextPutAll: '  -e Do not ignore exceptions, but open a Debugger'; newLine;
		nextPutAll: '  -r <feature> Require <feature>'; newLine;
		nextPutAll: '  -d <code> Evaluate <code>'; newLine;
		nextPutAll: '  -l <codeFile> Load e.g. file in <codeFile>'; newLine;
		nextPutAll: '  -s <scriptFile> Evaluate code in <scriptFile>'; newLine;
		nextPutAll: '  -- end of options; stop processing options'; newLine;
		newLine;
		nextPutAll: 'Arguments for scripts, code files, packages (and <code>):'; newLine;
		nextPutAll: '  are available in ''Smalltalk startUpScriptArguments'''; newLine;
		newLine;
		nextPutAll: 'Multiple options:'; newLine;
		nextPutAll: '  any option can be passed multiple times'; newLine ]
	on: UnhandledError
	do: [ :exception | exception return ]! !

!SystemDictionary methodsFor: 'startup' stamp: 'KLG 1/11/2023 22:27:13'!
displaySystemInformationStringOn: aStream
	"Display the system information string on standard output."

	[ aStream
		nextPutAll: 'System information:';		newLine;
		nextPutAll: self systemInformationString;		newLine ]
	on: UnhandledError
	do: [ :exception | exception return ]! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 1/13/2023 16:07:44'!
processCommandLineOption: optionCharacter optionArgument: optionArgument raiseExceptions: aBoolean
	"
	A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment):
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -h
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -v
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -q
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -e -d''0 zope''
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -r YAXO -rJSON -d ''Smalltalk startUpScriptArguments print'' xx YY 3492 -d'' 'here comes another' print'' -d ''{'The answer is: '. Smalltalk startUpScriptArguments first asNumber + Smalltalk startUpScriptArguments second asNumber} print'' 3 4 -- -d'' 'not this' print' ''

Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -rRequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st param1ToScript param2ToSCript ''parametro 3 to script ->>>--// 3''
	"
	"You can use 'Smalltalk startUpScriptArguments' inside the startUp script:"


	| exceptionToIgnoreOrNil entry |
	exceptionToIgnoreOrNil _ aBoolean ifFalse: [UnhandledError] ifTrue: [nil].
	optionCharacter caseOf: {
		[ $h ] -> [
			self displayCommandLineUsageOn: StdIOWriteStream stdout.
			self quit ].

		[ $v ] -> [ 
			self displaySystemInformationStringOn: StdIOWriteStream stdout.
			self quit ].

		[ $q ] -> [ self quit ].

		[ $r ] -> [		"as in 		-rRequiredFeature2        or         -r RequiredFeature2"
			{ 'Feature require: '. optionArgument } print.
			[ Feature require: optionArgument ] on: exceptionToIgnoreOrNil do: [:ex | ex return] ].

		[ $d ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
			{ 'Compiler evaluate: '. optionArgument } print.
			[ Compiler evaluate: optionArgument ] on: exceptionToIgnoreOrNil do: [:ex | ex return] ].

		[$l ] -> ["file in the file"
			{ 'File in: '. optionArgument} print.
			[(CodeFile newFromFile: optionArgument asFileEntry) fileIn] on: exceptionToIgnoreOrNil do: [:ex | ex return]].

		[ $s ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parameter to script script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
			{ 'Compiler evaluate contents of file named: '. optionArgument. ' arguments: '. Smalltalk startUpScriptArguments } print.
			entry _ optionArgument asFileEntry.
			entry exists ifTrue: [
				entry readStreamDo: [ :stream |
					[ Compiler evaluate: stream contentsOfEntireFile ] on: exceptionToIgnoreOrNil do: [:ex | ex return]]]].
	}
	otherwise: [].! !


!SystemDictionary methodsFor: 'startup' stamp: 'jmv 1/13/2023 15:57:38'!
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
					startUpScriptArguments _ nil ]
			].
		optionIndex _ nextOptionIndex ].! !

!methodRemoval: SystemDictionary #processCommandLineArgument:storeStartUpScriptArgsOn:exceptionToIgnore: stamp: 'jmv 1/13/2023 15:41:47'!
SystemDictionary removeSelector: #processCommandLineArgument:storeStartUpScriptArgsOn:exceptionToIgnore:!
