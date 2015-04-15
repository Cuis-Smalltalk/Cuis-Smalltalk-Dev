'From Cuis 4.2 of 25 July 2013 [latest update: #2239] on 14 April 2015 at 6:16:52.563511 pm'!
!classDefinition: #SystemDictionary category: #'System-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments startUpArguments '
	classVariableNames: 'EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore ShutDownList SourceFileVersionString SpecialSelectors StartUpList StartupStamp '
	poolDictionaries: ''
	category: 'System-Support'!

!Object methodsFor: 'message handling' stamp: 'jmv 4/14/2015 15:22'!
perform: selector withPossiblyWrongSizedArguments: argArray
	"Fix arguments size if needed"

	| arguments numArgs |
	numArgs _ selector numArgs.
	numArgs = argArray size
		ifTrue: [
			arguments _ argArray ]
		ifFalse: [
			arguments _ Array new: numArgs.
			arguments fillWith: argArray ].
	^ self perform: selector withArguments: arguments ! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'jmv 4/14/2015 15:22'!
fillWith: aCollection

	self replaceFrom: 1 to: (self size min: aCollection size) with: aCollection startingAt: 1 ! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 4/14/2015 14:49'!
saveAs: newName andQuit: aBoolean
	"Save the image  under a new name."

	self currentChangesName ifNotNil: [ :oldChangesName | | newChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FileDirectory default 
			copyFileWithoutOverwriteConfirmationNamed: oldChangesName
			toFileNamed: newChangesName.
		self setMacFileInfoOn: newChangesName ].

	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: aBoolean! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 4/14/2015 16:39'!
processCommandLineArgument: rawArgStream storeStartUpScriptArgsOn: startUpScriptArgs
	"
	Smalltalk processCommandLineArguments
	
	A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment):
	Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -rRequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3''
	"
	| p data |
	p _ rawArgStream next.

	(p first = $- and: [ p size > 1 ]) ifTrue: [
		"If the command is not included in p, it is next argument"
		p size = 2
			ifTrue: [
				"as in 		-r RequiredFeature1"
				data _ rawArgStream next ]
			ifFalse: [
				"as in 		-rRequiredFeature2"
				data _ p copyFrom: 3 to: p size ].
		p second caseOf: {
			[ $r ] -> [		"as in 		-rRequiredFeature2"
				{ 'Feature require: '. data } print.
				[ Feature require: data ] on: Error do: [] ].
			[ $d ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
				{ 'Compiler evaluate: '. data } print.
				[ Compiler evaluate: data ] on: Error do: [] ].
			[ $s ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
				[ rawArgStream atEnd ] whileFalse: [
					startUpScriptArgs nextPut: rawArgStream next ].
				"Can use 'Smalltalk startUpScriptArguments' inside the startUp script
				{ 'Compiler evaluate contents of file named: '. data. ' arguments: '. Smalltalk startUpScriptArguments } print."
				(FileStream oldFileOrNoneNamed: data) ifNotNil: [ :stream |
					[ Compiler evaluate: stream contentsOfEntireFile ] on: Error do: []].
				"Maybe we decide to clear them after startup script execution
				startUpScriptArguments _ nil" ]
		}
		otherwise: []
	]! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 4/14/2015 16:36'!
readCommandLineArguments
	"
	Smalltalk readCommandLineArguments
	"
	| arg i |
	startUpArguments _ Array streamContents: [ :strm |
		i _ 2.
		[ i <= 1000 and: [ (arg _ self getSystemAttribute: i) notNil ] ] whileTrue: [
			strm nextPut: arg.
			i _ i + 1 ]]! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 4/14/2015 15:06'!
startUpArguments
	"To make all raw command line arguments available to applications"

	^startUpArguments! !


!Symbol methodsFor: 'system primitives' stamp: 'jmv 4/14/2015 15:15'!
numArgs: n
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ 
	
	Note: Unrelated, but remember that we inherit #numArgs " 

	| selector numArgs aStream offs |
	
	selector _ self.
	(numArgs _ selector numArgs) >= n ifTrue: [ ^self ].	
	aStream _ WriteStream on: (String new: 16).
	aStream nextPutAll: self.
	
	(numArgs = 0) ifTrue: [ aStream nextPutAll: ':'. offs _ 0 ] ifFalse: [offs _ 1].
	2 to: n - numArgs + offs do: [ :i | aStream nextPutAll: 'with:' ].	
	^aStream contents asSymbol! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 4/14/2015 14:49'!
saveAs: newName
	"Save the image  under a new name."

	self saveAs: newName andQuit: false! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 4/14/2015 16:39'!
processCommandLineArguments
	"
	Smalltalk processCommandLineArguments
	"
	| rawArgStream |
	startUpScriptArguments _ Array streamContents: [ :strm |
		rawArgStream _ startUpArguments readStream.
		[ rawArgStream atEnd ] whileFalse: [
			self processCommandLineArgument: rawArgStream storeStartUpScriptArgsOn: strm ]]! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 4/14/2015 16:28'!
startup: resuming
	"
	resuming
		true: system is coming up (VM and image just started)
		false: we have just saved an image snapshot, but didn't quit.
	"

	"Here, startup begins!!"
	Cursor normal show.
	self setGCParameters.
	resuming ifTrue: [
		self clearExternalObjects ].
	self readCommandLineArguments.
	self processStartUpList: resuming.
	resuming ifTrue: [
		self setPlatformPreferences.
		self setStartupStamp.
		self processCommandLineArguments ].! !

!methodRemoval: SystemDictionary #processCommandLineArgument:!
SystemDictionary removeSelector: #processCommandLineArgument:!
!classDefinition: #SystemDictionary category: #'System-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments startUpArguments'
	classVariableNames: 'EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore ShutDownList SourceFileVersionString SpecialSelectors StartUpList StartupStamp'
	poolDictionaries: ''
	category: 'System-Support'!
