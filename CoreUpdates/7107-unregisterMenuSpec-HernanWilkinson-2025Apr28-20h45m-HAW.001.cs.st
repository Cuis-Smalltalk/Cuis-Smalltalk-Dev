'From Cuis7.3 [latest update: #7102] on 28 April 2025 at 8:59:40 pm'!

!SystemWindow class methodsFor: 'menu and shortcuts' stamp: 'HAW 4/28/2025 20:56:13'!
at: aWidgetKey unregisterMenuAndShortcutsSpecProvider: aMessageSend

	menuNshortcutsProviders ifNotNil: [ 
		(menuNshortcutsProviders at: aWidgetKey ifAbsent: [ ^self ]) removeKey: aMessageSend ]! !

