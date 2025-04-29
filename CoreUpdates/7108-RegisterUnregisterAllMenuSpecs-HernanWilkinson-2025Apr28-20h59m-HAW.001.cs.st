'From Cuis7.3 [latest update: #7102] on 29 April 2025 at 8:24:45 am'!

!SystemWindow class methodsFor: 'menu and shortcuts' stamp: 'HAW 4/28/2025 21:11:10'!
registerAll: specs

	specs do: [ :aSpec | self at: aSpec first registerMenuAndShortcutsSpecProvider: aSpec second ].
! !

!SystemWindow class methodsFor: 'menu and shortcuts' stamp: 'HAW 4/28/2025 21:11:32'!
unregisterAll: specs

	specs do: [ :aSpec | self at: aSpec first unregisterMenuAndShortcutsSpecProvider: aSpec second ].
! !

