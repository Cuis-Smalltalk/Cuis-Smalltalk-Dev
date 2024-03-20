'From Cuis6.3 [latest update: #6283] on 20 March 2024 at 3:26:04 pm'!

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 3/20/2024 15:23:31'!
processCommandLineOption: optionString optionArgument: optionArgument raiseExceptions: aBoolean
	"
	A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment):
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -h
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -v
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -q
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -e -d''0 zope''
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -r YAXO -rJSON -d ''Smalltalk startUpScriptArguments print'' xx YY 3492 -d'' 'here comes another' print'' -d ''{'The answer is: '. Smalltalk startUpScriptArguments first asNumber + Smalltalk startUpScriptArguments second asNumber} print'' 3 4 -- -d'' 'not this' print' ''

Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -r RequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d ''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st param1ToScript param2ToSCript ''parametro 3 to script ->>>--// 3''
	"
	"You can use 'Smalltalk startUpScriptArguments' inside the startUp script:"


	| exceptionToIgnoreOrNil file directory |
	exceptionToIgnoreOrNil := aBoolean ifFalse: [UnhandledError] ifTrue: [nil].
	optionString caseOf: {
		[ '-h' ] -> [
			self displayCommandLineUsageOn: StdIOWriteStream stdout.
			self quit ].

		[ '-v' ] -> [ 
			self displaySystemInformationStringOn: StdIOWriteStream stdout.
			self quit ].

		[ '-q' ] -> [ self quit ].

		[ '-r' ] -> [		"as in 		-rRequiredFeature2        or         -r RequiredFeature2"
			{ 'Feature require: '. optionArgument } print.
			[ Feature require: optionArgument ] on: exceptionToIgnoreOrNil do: [:ex | ex return] ].

		[ '-d' ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
			{ 'Compiler evaluate: '. optionArgument } print.
			[ Compiler evaluate: optionArgument ] on: exceptionToIgnoreOrNil do: [:ex | ex return] ].

		['-l' ] -> ["file in the file"
			file := optionArgument isAbsolutePathName
				ifTrue: [ FileEntry withAbsolutePathName: optionArgument ]
				ifFalse: [FileEntry withRelativePathName: optionArgument to: DirectoryEntry currentDirectory ].
			{ 'File in: '. optionArgument. ' -> '. file} print.
			[file readStreamDo: [ :stream | stream fileIn ]] on: exceptionToIgnoreOrNil do: [ :ex | ex return]].

		[ '-s' ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parameter to script script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
			file := optionArgument isAbsolutePathName
				ifTrue: [ FileEntry withAbsolutePathName: optionArgument ]
				ifFalse: [FileEntry withRelativePathName: optionArgument to: DirectoryEntry currentDirectory ].
			{ 'Compiler evaluate contents of file named: '. optionArgument. ' -> '. file. ' arguments: '. self startUpScriptArguments } print.
			file exists ifTrue: [
				file readStreamDo: [ :stream |
					[ Compiler evaluate: stream contentsOfEntireFile ] on: exceptionToIgnoreOrNil do: [ :ex | ex return]]]].

		[ '-ud' ] -> [
			directory := optionArgument asDirectoryEntry.
			DirectoryEntry setUserBaseDirectory: directory ].

		[ '-udIsBase' ] -> [ DirectoryEntry setUserBaseDirectoryAsCuisBase ].
	}
	otherwise: [].! !

