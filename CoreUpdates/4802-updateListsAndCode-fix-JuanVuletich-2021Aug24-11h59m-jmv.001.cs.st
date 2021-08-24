'From Cuis 5.0 [latest update: #4799] on 24 August 2021 at 12:00:18 pm'!

!CodeWindow methodsFor: 'updating' stamp: 'jmv 8/24/2021 11:42:18'!
updateListsAndCode
	"All code windows receive this message on any code change in the system"

	model ifNotNil: [
		self canDiscardEdits ifTrue: [
			self allMorphsDo: [ :m | (m is: #PluggableListMorph) ifTrue: [ m verifyContents ]].
			model updateIfNeeded ]]! !


!VersionsBrowserWindow methodsFor: 'updating' stamp: 'jmv 8/24/2021 11:42:36'!
updateListsAndCode
	"All code windows receive this message on any code change in the system"

	model ifNotNil: [
		model updateIfNeeded ]! !

