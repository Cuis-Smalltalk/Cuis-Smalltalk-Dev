'From Cuis7.1 [latest update: #6524] on 18 July 2024 at 11:39:41 am'!

!Feature class methodsFor: 'convenience' stamp: 'jmv 7/18/2024 10:59:15'!
requireInformFaiure: featureNameOrFilename
	"
	Answer whether the request is fulfilled.
	Feature require: 'StyledTextInstaller'
	Feature require: 'Sound'
	Feature require: 'Tests'
	"
	^ [ self require: featureNameOrFilename ]
		on: FeatureRequirementUnsatisfied
		do: [ :ex | PopUpMenu inform: ex messageText. false ]! !


!SystemDictionary methodsFor: 'startup' stamp: 'jmv 7/18/2024 11:11:36'!
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

		[ '-u' ] -> [ ChangeSet installNewUpdates ].

		[ '-r' ] -> [		"as in 		-rRequiredFeature2        or         -r RequiredFeature2"
			{ 'Feature require: '. optionArgument } print.
			[ Feature require: optionArgument ] on: FeatureRequirementUnsatisfied do: [ :ex |
				StdIOWriteStream stdout nextPutAll: ex messageText; newLine; flush.
				ex messageText print.
				self quit ] ].

		[ '-d' ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
			{ 'Compiler evaluate: '. optionArgument } print.
			[ Compiler evaluate: optionArgument ] on: exceptionToIgnoreOrNil do: [ :ex | ex return] ].

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


!NegativePowerError methodsFor: 'exceptionDescription' stamp: 'jmv 7/18/2024 11:04:13'!
defaultAction

	"Disable this preference to have Float nan answer (if Float receiver or argument) or Error message"
	| answer |
	(Preferences at: #askToInstallComplexPackage) ifTrue: [
		answer := PopUpMenu
			withCaption:
'Square (or even) Root of a negative Number:
Complex number support is not loaded
Would you like me to load it for you now?'
			chooseFrom: #(
				'Load Complex package'
				'Do not load Complex package'
				'Do not load Complex package and don''t ask again').
		answer = 1 ifTrue: [
			Feature requireInformFaiure: #'Complex'.
			Smalltalk at: #Complex ifPresent: [ :cplx |
				^ (cplx basicReal: receiver imaginary: 0) perform: selector withArguments: arguments ]].
		answer = 3 ifTrue: [
			(Preferences at: #askToInstallComplexPackage put: false)]].
	^ super defaultAction! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 7/18/2024 11:04:17'!
loadAdditionalThemes
	"Load additional themes and reopen menu."
	Feature requireInformFaiure: #'Theme-Themes'.
	self changeTheme! !


!TheWorldMenu methodsFor: 'menu' stamp: 'jmv 7/18/2024 11:04:21'!
openCommentGuide
	"In ClassCommentBrowser.pck"
	Smalltalk at: #CommentGuideWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Class Comment Guide is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature requireInformFaiure: #'ClassCommentBrowser'] 
	].
	Smalltalk at: #CommentGuide ifPresent: [ :cls | ^cls open ].
	self inform: 'Please install optional package ClassCommentBrowser.pck.st', String newLineString, 'Feature require: #''ClassCommentBrowser'' '! !

!TheWorldMenu methodsFor: 'menu' stamp: 'jmv 7/18/2024 11:04:24'!
openPackageDownloader
	"In PackageDownloader.pck"
	Smalltalk at: #PackageDownloaderWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Package Downloader is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature requireInformFaiure: #'PackageDownloader'] 
	].
	Smalltalk at: #PackageDownloaderWindow ifPresent: [ :cls | ^cls open ].
	self inform: 'Please install optional package PackageDownloader.pck.st', String newLineString, 'Feature require: #''PackageDownloader'' '! !

!TheWorldMenu methodsFor: 'menu' stamp: 'jmv 7/18/2024 11:04:28'!
openPackageInstaller
	"In PackageInstaller.pck"
	Smalltalk at: #PackageInstallerWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Package Installer is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature requireInformFaiure: #PackageInstaller ] 
	].
	Smalltalk at: #PackageInstallerWindow ifPresent: [ :cls | ^cls open ].
	self inform: 'Please install optional package PackageInstaller.pck.st', String newLineString, 'Feature require: #''PackageInstaller'' '! !

!TheWorldMenu methodsFor: 'menu' stamp: 'jmv 7/18/2024 11:04:33'!
openTerseGuide
	"In TerseGuide.pck"
	Smalltalk at: #TerseGuideWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Terse Guide is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature requireInformFaiure: #'TerseGuide'] 
	].
	Smalltalk at: #TerseGuideWindow ifPresent: [ :cls | ^cls openTerseGuide ].
	self inform: 'Please install optional package TerseGuide.pck.st', String newLineString, 'Feature require: #''TerseGuide'' '! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 7/18/2024 11:01:37'!
require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load 
	  [1] Build transitive closure as load order where Feature comes 
	       before its required features."
	packagesToLoad := self
			requireUnlessIn: OrderedCollection new
			main: nil
			requiringFeature: nil
			allRequiringFeatures: #().
	
	"Transcript show: 'packagesToLoad: '; newLine.
	packagesToLoad do: [ :x |Transcript show: x; newLine ].
	Transcript newLine."
	
	"[2] Check to see that each requirement is actually satisfied"
	packagesToLoad do: [ :fReq | fReq checkRequirement ].

	"[3] Load required packages before packages that require them"
	packagesToLoad reverseDo: [ :requirement |
		requirement isAlreadySatisfied ifFalse: [
			requirement install ]].
	self isAlreadySatisfied ifFalse: [
		self install ]! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 7/18/2024 11:02:19'!
satisfyRequirementsAndInstall
	"Like #require, but install me even if already satisified (i.e. installed)"

	| packagesToLoad |
		
	"Preflight before load"
	packagesToLoad := self
			requireUnlessIn: OrderedCollection new
			main: self
			requiringFeature: self
			allRequiringFeatures: #().
	
	"Check to see that each requirement is actually satisfied"
	packagesToLoad do: [ :fReq | fReq checkRequirement ].
	
	"All requirements are satisfied; do the deed"
	packagesToLoad reverseDo: [ :requirement |
		requirement isAlreadySatisfied ifFalse: [
			requirement install ]].
	self install! !


!CodePackageFile class methodsFor: 'services' stamp: 'jmv 7/18/2024 11:03:26'!
installPackage: aFileEntry

	| fullName pkName req |
	fullName := aFileEntry pathName.
	pkName := CodePackageFile packageNameFrom: fullName.
	req := (FeatureRequirement name: pkName)
		pathName: fullName.
	"Install with all requirements"
	[ req satisfyRequirementsAndInstall ]
		on: FeatureRequirementUnsatisfied
		do: [ :ex | PopUpMenu inform: ex messageText ]! !

!methodRemoval: FeatureRequirementUnsatisfied #defaultAction stamp: 'jmv 7/18/2024 11:33:51'!
FeatureRequirementUnsatisfied removeSelector: #defaultAction!

!FeatureRequirementUnsatisfied reorganize!
('as yet unclassified')
!

