'From Cuis 5.0 of 7 November 2016 [latest update: #3323] on 13 May 2018 at 6:53:04 pm'!

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/13/2018 18:48:20'!
add: aString action: aSymbol balloonText: stringOrText
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^(self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: #())
			setBalloonText: stringOrText! !


!MessageSetWindow methodsFor: 'menu commands' stamp: 'jmv 5/13/2018 18:52:01'!
filterMessageList
	"Allow the user to refine the list of messages."

	| aMenu |
	model messageList size <= 1 
		ifTrue: [ ^self inform: 'this is not a propitious filtering situation' ].

	aMenu _ MenuMorph new defaultTarget: model.
	aMenu addTitle: 'Filter by only showing...'.
	aMenu addStayUpIcons.
	aMenu 
		add: 'unsent messages' action: #filterToUnsentMessages balloonText: 'filter to show only messages that have no senders';
		addLine;
		add: 'messages that send...' action: #filterToSendersOf balloonText: 'filter to show only messages that send a selector I specify';
		add: 'messages that do not send...' action: #filterToNotSendersOf balloonText: 'filter to show only messages that do not send a selector I specify';
		addLine;
		add: 'messages whose selector is...' action: #filterToImplementorsOf balloonText: 'filter to show only messages with a given selector I specify';
		add: 'messages whose selector is NOT...' action: #filterToNotImplementorsOf balloonText: 'filter to show only messages whose selector is NOT a seletor I specify';
		addLine;
		add: 'messages in any change set' action: #filterToAnyChangeSet balloonText: 'filter to show only messages that occur in at least one change set';
		add: 'messages not in any change set' action: #filterToNotAnyChangeSet balloonText: 'filter to show only messages that do not occur in any change set in the system';
		addLine;
		add: 'messages authored by me' action: #filterToCurrentAuthor balloonText: 'filter to show only messages whose authoring stamp has my initials';
		add: 'messages not authored by me' action: #filterToNotCurrentAuthor balloonText: 'filter to show only messages whose authoring stamp does not have my initials';
		addLine;
		add: 'messages logged in .changes file' action: #filterToMessagesInChangesFile balloonText: 'filter to show only messages whose latest source code is logged in the .changes file';
		add: 'messages only in .sources file' action: #filterToMessagesInSourcesFile balloonText: 'filter to show only messages whose latest source code is logged in the .sources file';
		addLine;
		add: 'messages with prior versions' action: #filterToMessagesWithPriorVersions balloonText: 'filter to show only messages that have at least one prior version';
		add: 'messages without prior versions' action: #filterToMessagesWithoutPriorVersions balloonText: 'filter to show only messages that have no prior versions';
		addLine;
		add: 'uncommented messages' action: #filterToUncommentedMethods balloonText: 'filter to show only messages that do not have comments at the beginning';
		add: 'commented messages' action: #filterToCommentedMethods balloonText: 'filter to show only messages that have comments at the beginning'.
	aMenu popUpInWorld: self world! !


!MessageNamesWindow methodsFor: 'menu building' stamp: 'jmv 5/13/2018 18:17:58'!
selectorListMenu
	"Answer the menu associated with the selectorList"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	(aMenu add: 'senders (n)' action: #browseSenders icon: #mailForwardIcon)
		setBalloonText: 'browse senders of the chosen selector'.
	^ aMenu! !


!ProcessBrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/13/2018 18:44:18'!
processListMenu
	| menu rules |
	menu _ MenuMorph new defaultTarget: self.

	model selectedProcess
		ifNotNil: [ :selectedProcess |
			rules _ model class rulesFor: model selectedProcess.
			menu
				add: 'inspect (i)'				action: #inspectProcess 				icon: #inspectIcon;
				add: 'explore (I)'				action: #exploreProcess 				icon: #exploreIcon;
				add: 'references finder'	action: #openReferencesFinder 		icon: #exploreIcon.
			rules first
				ifTrue: [
					menu add: 'terminate (t)' target: model action: #terminateProcess icon: #cancelIcon.
					selectedProcess isSuspended
						ifTrue: [menu add: 'resume (r)' target: model action: #resumeProcess icon: #mediaPlaybackStartIcon]
						ifFalse: [menu add: 'suspend (s)' target: model action: #suspendProcess icon: #chatIcon]].
			rules second
				ifTrue: [
					menu
						add: 'change priority (p)'	action: #changePriority 	icon: #systemMonitorIcon;
						add: 'debug (d)'				action: #debugProcess 	icon: #debugIcon ].
			(selectedProcess suspendingList isKindOf: Semaphore)
				ifTrue: [menu add: 'signal Semaphore (S)' target: model action: #signalSemaphore icon: #haloHelpIcon ].
			menu add: 'full stack (k)' target: model action: #moreStack icon: #systemIcon.
			menu addLine].

	menu
		add: 'find context... (f)'					action: #findContext 	icon: #findIcon;
		add: 'find again (g)'	target: model 	action: #nextContext 	icon: #systemIcon.
	menu addLine.

	isStepping
		ifTrue: [ 	menu add: 'turn off auto-update (a)' 	action: #toggleAutoUpdate icon: #cancelIcon ]
		ifFalse: [ 	menu add: 'turn on auto-update (a)' 	action: #toggleAutoUpdate icon: #updateIcon ].
		
	menu add: 'update list (u)' target: model action: #updateProcessList icon: #updateIcon.

	menu addLine.
	CPUWatcher isMonitoring
			ifTrue: [ menu add: 'stop CPUWatcher' action: #stopCPUWatcher icon: #inspectIcon ]
			ifFalse: [ menu add: 'start CPUWatcher' action: #startCPUWatcher icon: #inspectIcon ].
	StackSizeWatcher isWatching
			ifTrue: [ menu add: 'stop StackSizeWatcher' action: #stopStackSizeWatcher icon: #inspectIcon ]
			ifFalse: [ menu add: 'start StackSizeWatcher' action: #startStackSizeWatcher  icon: #inspectIcon ].

	^ menu! !

!ProcessBrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/13/2018 18:25:38'!
stackListMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	model selectedContext
		ifNil: [^ aMenu].
	aMenu
		add: 'inspect context (c)' 		action: #inspectContext icon: #inspectIcon;
		add: 'explore context (C)' 		action: #exploreContext icon: #exploreIcon;
		add: 'inspect receiver (i)' 		action: #inspectReceiver icon: #inspectIcon;
		add: 'explore receiver (I)' 		action: #exploreReceiver icon: #exploreIcon;
		add: 'browse (b)' 				action: #browseContext icon: #editFindReplaceIcon.
	^aMenu! !


!TranscriptMorph methodsFor: 'menus' stamp: 'jmv 5/13/2018 18:22:44'!
getMenu
	"Set up the menu to apply to the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	doImmediateUpdates
		ifTrue: [ aMenu add: 'Only update in the regular Morphic cycle' 	action: #doRegularUpdates ]
		ifFalse: [ aMenu add: 'Immediately show each entry'	 				action: #doImmediateUpdates ].
	aMenu
		addLine;
		add: 'Workspace with Contents' 	action: #editContents;
		addLine;
		add: 'Clear Transcript' 							action: #clearInternal;
		add: 'Clear Transcript File' 						action: #clearFile;
		add: 'Clear Both' 								action: #clearAll;
		addLine.
	Transcript logsToFile
		ifTrue: [ aMenu add: 'Stop logging to File' 	action: #dontLogToFile ]
		ifFalse: [ aMenu add: 'Start logging to File' 	action: #logToFile ].
	^ aMenu! !

