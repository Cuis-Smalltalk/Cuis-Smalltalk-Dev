'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 4 November 2008 at 12:29:55 pm'!!classDefinition: #OldMorphWithModel category: #'Morphic-OldKernel'!OldBorderedMorph subclass: #OldMorphWithModel	instanceVariableNames: 'model slotName open '	classVariableNames: 'TimeOfError '	poolDictionaries: ''	category: 'Morphic-OldKernel'!!classDefinition: #OldScrollPane category: #'Morphic-OldWindows'!OldMorphWithModel subclass: #OldScrollPane	instanceVariableNames: 'scrollBar scroller retractableScrollBar getMenuSelector getMenuTitleSelector scrollBarHidden hasFocus hScrollBar '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-OldWindows'!!Parser methodsFor: 'error correction' stamp: 'jmv 11/3/2008 18:11'!correctSelector: proposedKeyword wordIntervals: spots exprInterval: expInt ifAbort: abortAction	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."	| alternatives aStream choice correctSelector lines firstLine |	"If we can't ask the user, assume that the keyword will be defined later"	self interactive ifFalse: [ ^ proposedKeyword asSymbol ].	alternatives _ Symbol possibleSelectorsFor: proposedKeyword.	aStream _ WriteStream on: (String new: 200).	aStream nextPutAll: (proposedKeyword contractTo: 35); cr.	firstLine _ 1. 	alternatives do:		[:sel | aStream nextPutAll: (sel contractTo: 35); nextPut: Character cr].	aStream nextPutAll: 'cancel'.	lines _ Array with: firstLine with: (alternatives size + firstLine).		choice _ (PopUpMenu labels: aStream contents lines: lines)		startUpWithCaption: 'Unknown selector, please confirm, correct, or cancel'.	(choice = 0) | (choice > (lines at: 2))		ifTrue: [			requestor selectionInterval isEmpty 				ifTrue: [ requestor selectFrom: spots first first to: spots last last ].			^ abortAction value ].	choice = 1 ifTrue: [ ^ proposedKeyword asSymbol ].	correctSelector _ alternatives at: choice - 1.	self substituteSelector: correctSelector keywords wordIntervals: spots.	((proposedKeyword last ~~ $:) and: [correctSelector last == $:]) ifTrue: [		^ abortAction value].	^ correctSelector.! !!Parser methodsFor: 'error correction' stamp: 'jmv 11/3/2008 18:10'!correctVariable: proposedVariable interval: spot	"Correct the proposedVariable to a known variable, or declare it as a new	variable if such action is requested.  We support declaring lowercase	variables as temps or inst-vars, and uppercase variables as Globals or 	ClassVars, depending on whether the context is nil (class=UndefinedObject).	Spot is the interval within the test stream of the variable.	rr 3/4/2004 10:26 : adds the option to define a new class. "	| tempIvar labels actions lines alternatives binding choice action start end |	"Check if this is an i-var, that has been corrected already (ugly)"	(encoder classEncoding instVarNames includes: proposedVariable) ifTrue: [		^LiteralVariableNode new 			name: proposedVariable index: (encoder classEncoding instVarNames indexOf: proposedVariable) - 1 type: 1;			yourself ].	"If we can't ask the user for correction, make it undeclared"	self interactive 		ifFalse: [ ^encoder undeclared: proposedVariable ].	"First check to see if the requestor knows anything about the variable"	tempIvar _ proposedVariable first isLowercase.	(tempIvar and: [ (binding _ requestor bindingOf: proposedVariable) notNil ])		ifTrue: [ ^encoder global: binding name: proposedVariable ].	"Build the menu with alternatives"	labels _ OrderedCollection new. actions _ OrderedCollection new. lines _ OrderedCollection new.	alternatives _ encoder possibleVariablesFor: proposedVariable.	tempIvar 		ifTrue: [ 			labels add: 'declare temp'. 			actions add: [ self declareTempAndPaste: proposedVariable ].			labels add: 'declare instance'.			actions add: [ self declareInstVar: proposedVariable ] ]		ifFalse: [ 			labels add: 'define new class'.			actions add: [self defineClass: proposedVariable].			labels add: 'declare global'.			actions add: [ self declareGlobal: proposedVariable ].			encoder classEncoding == UndefinedObject ifFalse: [ 				labels add: 'declare class variable'.				actions add: [ self declareClassVar: proposedVariable ] ] ].	lines add: labels size.	alternatives do: [ :each | 		labels add: each.		actions add: [ 			self				substituteWord: each 				wordInterval: spot 				offset: 0 				insideSelection: requestor selectionInterval notEmpty.			encoder encodeVariable: each ] fixTemps ].	lines add: labels size.	labels add: 'cancel'.	"Display the pop-up menu"	choice _ (PopUpMenu labelArray: labels asArray lines: lines asArray)		startUpWithCaption: 'Unknown variable: ', proposedVariable, ' please correct, or cancel:'.	action _ actions at: choice ifAbsent: [ 		start _ spot first.		end _ spot last.		requestor selectionInterval notEmpty ifTrue: [			start _ start + requestor startIndex - 1.			end _ end + requestor startIndex - 1].		requestor selectFrom: start to: end.		^self fail ].	"Execute the selected action"	^action value! !!Parser methodsFor: 'error correction' stamp: 'jmv 11/3/2008 18:10'!queryUndefined	| varStart varName labels caption | 		varName _ parseNode key.	caption _ varName , ' appears to beundefined at this point.Proceed anyway?'.	labels _ 'yesno'.	((PopUpMenu labels: labels) startUpWithCaption:		(caption asText makeBoldFrom: 1 to: varName size)) = 1 			ifFalse: [				varStart _ self endOfLastToken + requestorOffset - varName size + 1.				requestor selectionInterval notEmpty ifTrue: [					varStart _ varStart + requestor selectionInterval first -1].				requestor selectFrom: varStart to: varStart + varName size - 1.				^ self fail]! !!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 11/4/2008 09:20'!computeImageSegmentation
	"Smalltalk computeImageSegmentation"

	"Here's how the segmentation works:
For each partition, we collect the classes involved, and also all messages no longer used in the absence of this partition.  We start by computing a 'Miscellaneous' segment of all the unused classes in the system as is."

	| partitions unusedCandM newClasses expandedCandM |
	partitions := Dictionary new.
	unusedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						{ 
							}.
						{ 
							}}.
	partitions at: 'Miscellaneous' put: unusedCandM.
	newClasses := Array streamContents: 
					[:s | 
					(SystemOrganization categoriesMatching: 'VMConstruction-*') 
						do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'VMConstruction'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					(SystemOrganization categoriesMatching: 'ST80-*') 
						do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'ST80'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					(SystemOrganization categoriesMatching: 'Morphic-Games') 
						do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'Games'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					(SystemOrganization categoriesMatching: 'Morphic-Remote') 
						do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'Nebraska'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					((SystemOrganization categoriesMatching: 'Network-*') 
						copyWithoutAll: #('Network-Kernel' 'Network-Url' 'Network-Protocols' 'Network-ObjectSocket')) 
							do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'Network'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					(SystemOrganization categoriesMatching: 'Balloon3D-*') 
						do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'Balloon3D'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					(SystemOrganization categoriesMatching: 'FFI-*') 
						do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'FFI'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					(SystemOrganization categoriesMatching: 'Genie-*') 
						do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'Genie'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					(SystemOrganization categoriesMatching: 'Speech-*') 
						do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'Speech'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					#('Morphic-Components') 
						do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'Components'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					#('Sound-Scores' 'Sound-Interface') 
						do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	newClasses := newClasses 
				, #(#WaveletCodec #OldSonogram #FWT #AIFFFileReader).
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'Sound'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					((SystemOrganization categoriesMatching: 'Tools-*') 
						copyWithout: 'Tools-Menus') 
							do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	newClasses := newClasses 
				copyWithoutAll: #(#Debugger #Inspector #ContextVariablesInspector #SyntaxError #ChangeSet #ChangeRecord #ClassChangeRecord #ChangeList #VersionsBrowser).
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'Tools'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					(SystemOrganization categoriesMatching: 'Balloon-MMFlash*') 
						do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	newClasses := newClasses , #(#ADPCMCodec).
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'Flash'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					(SystemOrganization categoriesMatching: 'Balloon-TrueType*') 
						do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'TrueType'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	newClasses := Array streamContents: 
					[:s | 
					(SystemOrganization categoriesMatching: 'Graphics-Files') 
						do: [:cat | (SystemOrganization superclassOrder: cat) do: [:c | s nextPut: c name]]].
	expandedCandM := Smalltalk unusedClassesAndMethodsWithout: { 
						unusedCandM first asArray , newClasses.
						unusedCandM second}.
	partitions at: 'GraphicFiles'
		put: { 
				(expandedCandM first copyWithoutAll: unusedCandM first)
					addAll: newClasses;
					yourself.
				expandedCandM second copyWithoutAll: unusedCandM second}.
	unusedCandM := expandedCandM.
	#(#AliceConstants 'Balloon3D' #B3DEngineConstants 'Balloon3D' #WonderlandConstants 'Balloon3D' #FFIConstants 'FFI' #KlattResonatorIndices 'Speech') 
		pairsDo: [:poolName :part | (partitions at: part) first add: poolName].
	partitions keysDo: 
			[:k | 
			k = 'Miscellaneous' 
				ifFalse: 
					[(partitions at: 'Miscellaneous') first 
						removeAllFoundIn: (partitions at: k) first]].
	^partitions! !!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 11/4/2008 12:27'!presumedSentMessages   | sent |"Smalltalk presumedSentMessages"	"The following should be preserved for doIts, etc"	sent _ IdentitySet new.	#( rehashWithoutBecome compactSymbolTable rebuildAllProjects		browseAllSelect:  lastRemoval		vScrollBarValue: scrollBarMenuButtonPressed: 		withSelectionFrom:  to: removeClassNamed:		dragon: hilberts: mandala: web test3 factorial tinyBenchmarks benchFib		newDepth: restoreAfter: forgetDoIts zapAllMethods obsoleteClasses		removeAllUnSentMessages abandonSources removeUnreferencedKeys		reclaimDependents zapOrganization condenseChanges browseObsoleteReferences		subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:		methodsFor:stamp: methodsFor:stamp:prior: instanceVariableNames:		startTimerInterruptWatcher unusedClasses) do:		[:sel | sent add: sel].	"The following may be sent by perform: in dispatchOnChar..."	(ParagraphEditor classPool at: #CmdActions) asSet do:		[:sel | sent add: sel].	(ParagraphEditor classPool at: #ShiftCmdActions) asSet do:		[:sel | sent add: sel].	^ sent! !!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 11/4/2008 12:27'!removeAllUnSentMessages	"Smalltalk removeAllUnSentMessages"	"[Smalltalk unusedClasses do: [:c | (Smalltalk at: c) removeFromSystem]. 	Smalltalk removeAllUnSentMessages > 0] whileTrue."	"Remove all implementations of unsent messages."	| sels n |	sels _ self allUnSentMessages.	"The following should be preserved for doIts, etc"	"needed even after #majorShrink is pulled"	#(#rehashWithoutBecome #compactSymbolTable #rebuildAllProjects #browseAllSelect:  #lastRemoval vScrollBarValue: #scrollBarMenuButtonPressed: #withSelectionFrom: #to: #removeClassNamed: #dragon: #hilberts: #mandala: #web #test3 #factorial #tinyBenchmarks #benchFib #newDepth: #restoreAfter: #forgetDoIts #zapAllMethods #obsoleteClasses #removeAllUnSentMessages #abandonSources #removeUnreferencedKeys #reclaimDependents #zapOrganization #condenseChanges #browseObsoleteReferences #subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: #methodsFor:stamp: #methodsFor:stamp:prior: #instanceVariableNames: #startTimerInterruptWatcher #unusedClasses )		do: [:sel | sels				remove: sel				ifAbsent: []].	"The following may be sent by perform: in dispatchOnChar..."	(ParagraphEditor classPool at: #CmdActions) asSet		do: [:sel | sels				remove: sel				ifAbsent: []].	(ParagraphEditor classPool at: #ShiftCmdActions) asSet		do: [:sel | sels				remove: sel				ifAbsent: []].	sels size = 0		ifTrue: [^ 0].	n _ 0.	Smalltalk		allBehaviorsDo: [:x | n _ n + 1].	'Removing ' , sels size printString , ' messages . . .'		displayProgressAt: Sensor cursorPoint		from: 0		to: n		during: [:bar | 			n _ 0.			self				allBehaviorsDo: [:class | 					bar value: (n _ n + 1).					sels						do: [:sel | class basicRemoveSelector: sel]]].	^ sels size! !OldTextMorph removeSelector: #getAllButFirstCharacter!OldTextMorph removeSelector: #getFirstCharacter!OldTextMorph removeSelector: #installEditor!OldPluggableTextMorph removeSelector: #appendTextEtoy:!OldPluggableTextMorph removeSelector: #languagePrefs!OldPluggableTextMorph removeSelector: #recognizeCharacters!OldPluggableTextMorph removeSelector: #select!OldPluggableTextMorph removeSelector: #translateIt!OldScrollPane removeSelector: #hideScrollBar!OldScrollPane removeSelector: #hideScrollBarIndefinitely!OldScrollPane removeSelector: #scrollBarValue:!OldScrollPane removeSelector: #scrollbarWidth!OldScrollPane removeSelector: #totalScrollRange!OldScrollPane removeSelector: #unadjustedScrollRange!!classDefinition: #OldScrollPane category: #'Morphic-OldWindows'!OldMorphWithModel subclass: #OldScrollPane	instanceVariableNames: 'scrollBar scroller retractableScrollBar getMenuSelector getMenuTitleSelector hasFocus hScrollBar'	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-OldWindows'!!classDefinition: #OldMorphWithModel category: #'Morphic-OldKernel'!OldBorderedMorph subclass: #OldMorphWithModel	instanceVariableNames: 'model slotName open'	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-OldKernel'!OldMorph removeSelector: #addLocationsTo:zoomNScrollTo:coordinateSystemsTo:from:!OldMorph removeSelector: #externalize:in:!OldMorph removeSelector: #internalize:from:!Smalltalk removeClassNamed: #OldComponentLikeMorphWithModel!