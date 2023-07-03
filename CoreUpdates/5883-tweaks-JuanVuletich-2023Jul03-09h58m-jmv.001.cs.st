'From Cuis 6.0 [latest update: #5882] on 3 July 2023 at 10:02:13 am'!

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 7/3/2023 09:59:51'!
fileOutCurrentVersionsOfSelections

	self request: 'Enter file name' initialAnswer: 'CurrentVersions.st' do: [ :aString |
		aString asFileEntry writeStreamDo: [ :stream |
			stream timeStamp.
			self currentVersionsOfSelections print do: [ :methodRef |
				methodRef actualClass
					printMethodChunk: methodRef methodSymbol
					withPreamble: true
					on: stream
					moveSource: false
					toFile: 0 ]]].! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 7/3/2023 10:01:07'!
fileOutSelections

	self request: 'Enter file name' initialAnswer: 'SelectedChanges.st' do: [ :aString |
		aString asFileEntry writeStreamDo: [ :stream |
			stream timeStamp.
			listSelections with: changeList do: [ :selected :item |
				selected ifTrue: [ item fileOutOn: stream ]]]].! !


!ChangeListWindow methodsFor: 'menu commands' stamp: 'jmv 7/3/2023 09:59:43'!
browseCurrentVersionsOfSelections
	"Opens a message-list browser on the current in-memory versions of all methods that are currently seleted"

	| currentVersions |
	currentVersions := model currentVersionsOfSelections.
currentVersions print.
	currentVersions isEmpty ifTrue: [
		^ self inform: 'no selected methods have in-memory counterparts' ].
	MessageSetWindow
		openMessageListUnsorted: currentVersions
		label: 'Current versions of selected methods in ', model file localName.! !

