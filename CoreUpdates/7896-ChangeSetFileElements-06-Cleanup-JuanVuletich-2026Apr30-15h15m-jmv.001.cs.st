'From Cuis7.7 [latest update: #7890] on 30 April 2026 at 3:16:21 pm'!
!classDefinition: #VersionsBrowser category: #'Tools-Changes'!
ChangeList subclass: #VersionsBrowser
	instanceVariableNames: 'classOfMethod selectorOfMethod addedChangeRecord addedChange '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!methodRemoval: ChangeListElement #changeType stamp: 'jmv 30/Apr/2026 15:15:41'!
ChangeListElement removeSelector: #changeType!
!methodRemoval: VersionsBrowserWindow class #browseVersionsOf:class:meta:category:selector:extraChangeRecord: stamp: 'jmv 30/Apr/2026 15:15:30'!
VersionsBrowserWindow class removeSelector: #browseVersionsOf:class:meta:category:selector:extraChangeRecord:!
!methodRemoval: VersionsBrowserWindow class #browseVersionsOf:class:meta:category:selector:lostMethodPointer:extraChangeRecord: stamp: 'jmv 30/Apr/2026 15:15:35'!
VersionsBrowserWindow class removeSelector: #browseVersionsOf:class:meta:category:selector:lostMethodPointer:extraChangeRecord:!
!methodRemoval: ClassCommentVersionsBrowser class #commentRecordsOf: stamp: 'jmv 30/Apr/2026 15:15:51'!
ClassCommentVersionsBrowser class removeSelector: #commentRecordsOf:!
!methodRemoval: VersionsBrowser #addedChangeRecord: stamp: 'jmv 30/Apr/2026 15:15:51'!
VersionsBrowser removeSelector: #addedChangeRecord:!
!methodRemoval: ChangeList #scanSpecificChangeRecordType stamp: 'jmv 30/Apr/2026 15:15:51'!
ChangeList removeSelector: #scanSpecificChangeRecordType!
!classDefinition: #VersionsBrowser category: #'Tools-Changes'!
ChangeList subclass: #VersionsBrowser
	instanceVariableNames: 'classOfMethod selectorOfMethod addedChange'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
