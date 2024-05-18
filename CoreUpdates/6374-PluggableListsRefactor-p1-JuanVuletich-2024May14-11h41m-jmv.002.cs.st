'From Cuis7.1 [latest update: #6367] on 14 May 2024 at 11:59:26 am'!

!HierarchicalListMorph methodsFor: 'initialization' stamp: 'jmv 5/14/2024 11:45:33'!
mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel

	mainView := aMainView.
	menuGetter := getMenuSel.
	keystrokeActionSelector := keyActionSel.! !

!HierarchicalListMorph methodsFor: 'initialization' stamp: 'jmv 5/14/2024 11:45:55'!
model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel

	self model: anObject.
	getListSelector := getListSel.
	getSelectionSelector := getSelectionSel.
	setSelectionSelector := setSelectionSel.
	autoDeselect := true.
	self list: self getList.! !


!PluggableListMorph methodsFor: 'initialization' stamp: 'jmv 5/14/2024 11:46:43'!
mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel

	mainView := aMainView.
	menuGetter := getMenuSel.
	keystrokeActionSelector := keyActionSel.! !

!PluggableListMorph methodsFor: 'initialization' stamp: 'jmv 5/14/2024 11:49:46'!
model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel

	self model: anObject.
	getListSelector := getListSel.
	getIndexSelector := getSelectionSel.
	setIndexSelector := setSelectionSel.
	autoDeselect := true.
	self
		updateList;
		initForKeystrokes
! !


!PluggableListMorphOfMany methodsFor: 'initialization' stamp: 'jmv 5/14/2024 11:57:56'!
model: anObject listGetter: listSel primarySelectionGetter: getSelectionSel primarySelectionSetter: setSelectionSel listSelectionGetter: getListSel listSelectionSetter: setListSel
	"setup a whole load of pluggability options"
	getSelectionListSelector := getListSel.
	setSelectionListSelector := setListSel.
	self model: anObject listGetter: listSel indexGetter: getSelectionSel indexSetter: setSelectionSel! !


!HierarchicalListMorph class methodsFor: 'instance creation' stamp: 'jmv 5/14/2024 11:50:55'!
model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel;
		mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel! !


!PluggableListMorph class methodsFor: 'instance creation' stamp: 'jmv 5/14/2024 11:51:09'!
model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel;
		mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel! !


!PluggableListMorphOfMany class methodsFor: 'instance creation' stamp: 'jmv 5/14/2024 11:58:33'!
model: anObject listGetter: listSel primarySelectionGetter: getSelectionSel primarySelectionSetter: setSelectionSel listSelectionGetter: getListSel listSelectionSetter: setListSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel
	^self new
		model: anObject listGetter: listSel primarySelectionGetter: getSelectionSel primarySelectionSetter: setSelectionSel listSelectionGetter: getListSel listSelectionSetter: setListSel;
		mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel! !

