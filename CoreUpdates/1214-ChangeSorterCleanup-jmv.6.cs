'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 21 March 2012 at 3:04:50 pm'!
!classDefinition: #ChangeSorter category: #'Tools-Changes'!
CodeProvider subclass: #ChangeSorter
	instanceVariableNames: 'parent myChangeSet currentClassName currentSelector priorChangeSetList changeSetCategory '
	classVariableNames: 'AllChangeSets ChangeSetCategories PreviousSet '
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeSetCategory methodsFor: 'miscellaneous' stamp: 'jmv 3/21/2012 14:36'!
          reconstituteList
	"Clear out the receiver's elements and rebuild them"

	| newMembers |
	"First determine newMembers and check if they have not changed..."
	newMembers _ ChangeSorter allChangeSets select:
		[:aChangeSet | "ChangeSorter perform: membershipSelector with: aChangeSet" true].
	(newMembers collect: [:cs | cs name]) = keysInOrder ifTrue: [^ self  "all current"].

	"Things have changed.  Need to recompute the whole category"
	self clear.
	newMembers do:
		[:aChangeSet | self fasterElementAt: aChangeSet name asSymbol put: aChangeSet] 
! !


!ChangeSorter methodsFor: 'access' stamp: 'jmv 3/21/2012 14:41'!
                    labelString
	"The label for my entire window.  The large button that displays my name is gotten via mainButtonName"

	^ String streamContents:
		[:aStream |
			aStream nextPutAll: (ChangeSet current == myChangeSet
				ifTrue: ['Changes go to "', myChangeSet name, '"']
				ifFalse: ['ChangeSet: ', myChangeSet name])]! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 3/21/2012 14:43'!
                     changeSetList
	"Answer a list of ChangeSet names to be shown in the change sorter."

	^ (ChangeSorter allChangeSets collect: [ :a | a name ]) reversed! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 3/21/2012 14:38'!
                            newSet
	"Create a new changeSet and show it., making it the current one.  Reject name if already in use."

	| aSet |
	aSet _ self class newChangeSet.
	aSet ifNotNil: [
		self update.
		self showChangeSet: aSet.
		self changed: #relabel]! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 3/21/2012 15:04'!
      updateIfNecessary
	"Recompute all of my panes."

	| newList |

	myChangeSet ifNil: [^ self].  "Has been known to happen though shouldn't"
	myChangeSet isMoribund ifTrue: [
		self changed: #changeSetList.
		^ self showChangeSet: ChangeSet current ].

	newList _ self changeSetList.
	(priorChangeSetList == nil or: [priorChangeSetList ~= newList])
		ifTrue: [
			priorChangeSetList _ newList.
			self changed: #changeSetList]! !


!ChangeSorter class methodsFor: 'adding' stamp: 'jmv 3/21/2012 14:29'!
            newChangesFromStream: aStream named: aName
	"File in the code from the stream into a new change set whose
	name is derived from aName. Leave the 'current change set'
	unchanged. Return the new change set or nil on failure."

	| oldChanges newName newSet |
	oldChanges _ ChangeSet current.
	newName _ (aName prefixAndSuffix: $-)
		ifNotNil: [ :ary | ary first ]
		ifNil: [ aName sansPeriodSuffix ].
	newSet _ self basicNewChangeSet: newName.
	[
		newSet ifNotNil: [
			ChangeSet newChanges: newSet.
			aStream fileInAnnouncing: 'Loading ', newName, '...'.
			Transcript show: 'File ', aName, ' successfully filed in to change set ', newName; newLine].
		aStream close
	] ensure: [
		ChangeSet newChanges: oldChanges].
	^ newSet! !


!DualChangeSorter methodsFor: 'other' stamp: 'jmv 3/21/2012 14:42'!
labelString
	"The window label"

	| changesName |
	changesName _ 'Changes go to "', ChangeSet current name,  '"'.
	^ changesName! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 3/21/2012 14:34'!
                  presumedSentMessages
	| sent |
	"
	In addition to those here, if it is desired to preserve some methods from deletion, see #nominallyUnsent:
	Smalltalk presumedSentMessages
	"

	"The following should be preserved for doIts, etc"
	sent _ IdentitySet new.
	#( rehashWithoutBecome compactSymbolTable
		browseAllSelect:  lastRemoval
		vScrollBarValue: hScrollBarValue: 
		to: removeClassNamed:
		dragon: hilberts: mandala: web test3 factorial tinyBenchmarks benchFib
		newDepth: restoreAfter: zapAllMethods obsoleteClasses
		removeAllUnSentMessages abandonSources removeUnreferencedKeys
		zapOrganization condenseChanges browseObsoleteReferences
		subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		methodsFor:stamp: methodsFor:stamp:prior: instanceVariableNames:
		startTimerEventLoop unusedClasses
		unimplemented
		reduceCuis
		variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		printSpaceAnalysis:on:) do: [ :sel |
			sent add: sel].
	"The following may be sent by perform: in dispatchOnChar..."
	(TextEditor cmdActions) asSet do: [ :sel | sent add: sel].
	(SmalltalkEditor cmdActions) asSet do: [ :sel | sent add: sel].
	#(beReadOnlyBinding beReadWriteBinding) do: [ :sel |
		sent add: sel].
	^ sent! !

!methodRemoval: ChangeSorter class #allChangeSetNames!
ChangeSorter class removeSelector: #allChangeSetNames!
!methodRemoval: ChangeSorter class #belongsInAll:!
ChangeSorter class removeSelector: #belongsInAll:!
!methodRemoval: ChangeSorter class #belongsInNumbered:!
ChangeSorter class removeSelector: #belongsInNumbered:!
!methodRemoval: ChangeSorter class #changeSetCategoryClass!
ChangeSorter class removeSelector: #changeSetCategoryClass!
!methodRemoval: ChangeSorter class #changeSetCategoryNamed:!
ChangeSorter class removeSelector: #changeSetCategoryNamed:!
!methodRemoval: ChangeSorter class #changeSetsNamedSuchThat:!
ChangeSorter class removeSelector: #changeSetsNamedSuchThat:!
!methodRemoval: ChangeSorter class #highestNumberedChangeSet!
ChangeSorter class removeSelector: #highestNumberedChangeSet!
!methodRemoval: ChangeSorter class #promoteToTop:!
ChangeSorter class removeSelector: #promoteToTop:!
!methodRemoval: ChangeSorter class #removeChangeSetsNamedSuchThat:!
ChangeSorter class removeSelector: #removeChangeSetsNamedSuchThat:!
!methodRemoval: ChangeSorter class #removeEmptyUnnamedChangeSets!
ChangeSorter class removeSelector: #removeEmptyUnnamedChangeSets!
!methodRemoval: ChangeSorter #changeSetCategory!
ChangeSorter removeSelector: #changeSetCategory!
!methodRemoval: ChangeSorter #parenthesizedCategoryName!
ChangeSorter removeSelector: #parenthesizedCategoryName!
!methodRemoval: ChangeSorter #setDefaultChangeSetCategory!
ChangeSorter removeSelector: #setDefaultChangeSetCategory!
!classDefinition: #ChangeSorter category: #'Tools-Changes'!
CodeProvider subclass: #ChangeSorter
	instanceVariableNames: 'parent myChangeSet currentClassName currentSelector priorChangeSetList'
	classVariableNames: 'AllChangeSets'
	poolDictionaries: ''
	category: 'Tools-Changes'!
!methodRemoval: ChangeSetCategory #defaultChangeSetToShow!
ChangeSetCategory removeSelector: #defaultChangeSetToShow!
!methodRemoval: ChangeSetCategory #includesChangeSet:!
ChangeSetCategory removeSelector: #includesChangeSet:!
