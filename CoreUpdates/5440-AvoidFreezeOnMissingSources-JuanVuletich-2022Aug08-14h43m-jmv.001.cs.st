'From Cuis 6.0 [latest update: #5440] on 8 August 2022 at 2:43:18 pm'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 8/8/2022 14:43:10'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems.
	Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sources changes entry msg wmsg |

	"Do not open source files if internalized (i.e. notNil)"
	sources _ SourceFiles at: 1.
	sources ifNil: [
		entry _ Smalltalk defaultSourcesName asFileEntry.
		entry exists ifFalse: [
			entry _ Smalltalk alternativeSourcesName asFileEntry ].
		entry exists ifTrue: [
			sources _ [ entry readStream ] on: FileDoesNotExistException do: [ nil ]]].
	(sources isNil and: [ Preferences at: #warnIfNoSourcesFile ])
		ifTrue: [
				msg _ 'Cuis cannot locate the sources file named ' , entry pathName, '.', String newLineString,
					'Please check that the file is properly named and is in the same directory as this image.'.
				Smalltalk platformName = 'Mac OS' ifTrue: [
					msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			[
				(Delay forSeconds: 1) wait.
				self inform: msg
			] forkAt: Processor activePriority -1.
		].

	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ 
		entry _ Smalltalk defaultChangesName asFileEntry.
		[ entry appendStream ] on: FileWriteError do: [ nil ] ].
	(changes isNil and: [Preferences at: #warnIfNoChangesFile])
		ifTrue: [
			wmsg _ 'Cuis cannot write to the changes file named ' , entry pathName, '.', String newLineString, String newLineString,
				'Please check that you have write permission for this file.', String newLineString, String newLineString,
				'You won''t be able to save this image correctly until you fix this.'.
			[
				(Delay forSeconds: 1) wait.
				self inform: wmsg
			] forkAt: Processor activePriority -1 
		].
	ChangesInitialFileSize _ changes ifNotNil: [ changes position ].

	SourceFiles _ Array with: sources with: changes.! !

