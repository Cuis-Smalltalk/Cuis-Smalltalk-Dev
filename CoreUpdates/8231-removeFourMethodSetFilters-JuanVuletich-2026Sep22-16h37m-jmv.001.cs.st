'From Cuis7.9 [latest update: #8212] on 22 September 2026 at 4:38:11 pm'!

!MethodSetWindow methodsFor: 'menu commands' stamp: 'jmv 22/Sep/2026, 6:45:27 pm (UTC)'!
filterMessageList
	"Allow the user to refine the list of messages."

	| aMenu |
	model messageList size <= 1 
		ifTrue: [ ^self inform: 'this is not a propitious filtering situation' ].

	aMenu := MenuMorph new defaultTarget: model.
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
		add: 'uncommented messages' action: #filterToUncommentedMethods balloonText: 'filter to show only messages that do not have comments at the beginning';
		add: 'commented messages' action: #filterToCommentedMethods balloonText: 'filter to show only messages that have comments at the beginning'.
	aMenu popUpInWorld: self world! !

!methodRemoval: VersionsBrowser class #versionCountForSelector:class: stamp: 'jmv 22/Sep/2026, 7:37:35 pm (UTC)'!
VersionsBrowser class removeSelector: #versionCountForSelector:class:!
!methodRemoval: MethodSet #filterToMessagesInSourcesFile stamp: 'jmv 22/Sep/2026, 7:37:35 pm (UTC)'!
MethodSet removeSelector: #filterToMessagesInSourcesFile!
!methodRemoval: MethodSet #filterToMessagesWithoutPriorVersions stamp: 'jmv 22/Sep/2026, 7:37:35 pm (UTC)'!
MethodSet removeSelector: #filterToMessagesWithoutPriorVersions!
!methodRemoval: MethodSet #filterToMessagesInChangesFile stamp: 'jmv 22/Sep/2026, 7:37:35 pm (UTC)'!
MethodSet removeSelector: #filterToMessagesInChangesFile!
!methodRemoval: MethodSet #filterToMessagesWithPriorVersions stamp: 'jmv 22/Sep/2026, 7:37:35 pm (UTC)'!
MethodSet removeSelector: #filterToMessagesWithPriorVersions!

!VersionsBrowser class reorganize!
!

