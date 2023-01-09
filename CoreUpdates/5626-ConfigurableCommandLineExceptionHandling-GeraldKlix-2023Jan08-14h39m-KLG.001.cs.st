'From Haver 6.0 [latest update: #5625] on 8 January 2023 at 3:01:33 pm'!

!SystemDictionary methodsFor: 'startup' stamp: 'KLG 1/8/2023 14:58:13'!
processCommandLineArgument: rawArgStream storeStartUpScriptArgsOn: startUpScriptArgs exceptionToIgnore: exceptionToIgnore
	"
	Smalltalk processCommandLineArguments
	
	A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment):
		Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -rRequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3''

	Note: By default, any exceptions thrown during the evaluation of arguments are simply ignored. To ease debugging, use the -e argument, and set the exception to ignore to nil.
	This effectively means Do Not Ignore Exceptions. For example:
		squeak Cuis.image -e nil -s myScript.st
	If ``myScript.st`` contains an error, a Debugger will pop up.
	"
	| p data entry nextExceptionToIgnore |
	nextExceptionToIgnore _ exceptionToIgnore.
	p _ rawArgStream next.

	(p size > 1 and: [p first = $-]) ifTrue: [
		"If the command is not included in p, it is next argument"
		p size = 2
			ifTrue: [
				"as in 		-r RequiredFeature1"
				data _ rawArgStream next ]
			ifFalse: [
				"as in 		-rRequiredFeature2"
				data _ p copyFrom: 3 to: p size ].
		p second caseOf: {
			[ $e ] -> [ nextExceptionToIgnore _ Compiler evaluate: data ":No exception handling here, this is meant for debugging."].
			[ $r ] -> [		"as in 		-rRequiredFeature2"
				{ 'Feature require: '. data } print.
				[ Feature require: data ] on: exceptionToIgnore do: [:ex | ex return] ].
			[ $d ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
				{ 'Compiler evaluate: '. data } print.
				[ Compiler evaluate: data ] on: exceptionToIgnore do: [:ex | ex return] ].
			[$l ] -> ["file in the file"
				{ 'File in: '. data} print.
				[(CodeFile newFromFile: data asFileEntry) fileIn] on: exceptionToIgnore do: [:ex | ex return]].
			[ $s ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
				[ rawArgStream atEnd ] whileFalse: [
					startUpScriptArgs nextPut: rawArgStream next ].
				"Can use 'Smalltalk startUpScriptArguments' inside the startUp script
				{ 'Compiler evaluate contents of file named: '. data. ' arguments: '. Smalltalk startUpScriptArguments } print."
				entry _ data asFileEntry.
				entry exists ifTrue: [
					entry readStreamDo: [ :stream |
						[ Compiler evaluate: stream contentsOfEntireFile ] on: exceptionToIgnore do: [:ex | ex return]]].
				"Maybe we decide to clear them after startup script execution
				startUpScriptArguments _ nil" ]
		}
		otherwise: []
	].
	^ nextExceptionToIgnore! !


!SystemDictionary methodsFor: 'startup' stamp: 'KLG 1/8/2023 14:59:46'!
processCommandLineArguments
	"
	Smalltalk processCommandLineArguments
	"
	| rawArgStream exceptionToIgnore |
	exceptionToIgnore _ UnhandledError.
	startUpScriptArguments _ Array streamContents: [ :strm |
		rawArgStream _ startUpArguments readStream.
		[ rawArgStream atEnd ] whileFalse: [
			exceptionToIgnore _ self
				processCommandLineArgument: rawArgStream
				storeStartUpScriptArgsOn: strm
				exceptionToIgnore: exceptionToIgnore ]]! !

!methodRemoval: SystemDictionary #processCommandLineArgument:storeStartUpScriptArgsOn: stamp: 'KLG 1/8/2023 14:59:54'!
SystemDictionary removeSelector: #processCommandLineArgument:storeStartUpScriptArgsOn:!
