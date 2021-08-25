'From Cuis 5.0 [latest update: #4808] on 25 August 2021 at 9:23:13 am'!

!CodeWindow methodsFor: 'updating' stamp: 'jmv 8/25/2021 09:13:01'!
updateListsAndCodeNow
	"All code windows receive this message on any code change in the system"
	self canDiscardEdits ifTrue: [
		self allMorphsDo: [ :m | (m is: #PluggableListMorph) ifTrue: [ m verifyContents ]].
		model updateIfNeeded ]! !


!VersionsBrowserWindow methodsFor: 'updating' stamp: 'jmv 8/25/2021 09:13:05'!
updateListsAndCodeNow
	"All code windows receive this message on any code change in the system"
	model updateIfNeeded! !


!CodeWindow methodsFor: 'updating' stamp: 'jmv 8/25/2021 09:21:47'!
updateListsAndCode
	"All code windows receive this message on any code change in the system.
	Process it only once, for the benefit of installing large packages!!"

	(self hasProperty: #updateListsAndCode) ifFalse: [
		self setProperty: #updateListsAndCode toValue: true.
		self whenUIinSafeState: [
			self removeProperty: #updateListsAndCode.
			self updateListsAndCodeNow ]].! !

!methodRemoval: VersionsBrowserWindow #updateListsAndCode stamp: 'jmv 8/25/2021 09:13:32'!
VersionsBrowserWindow removeSelector: #updateListsAndCode!
