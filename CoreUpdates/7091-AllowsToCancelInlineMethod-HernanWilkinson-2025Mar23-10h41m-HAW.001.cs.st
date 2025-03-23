'From Cuis7.3 [latest update: #7084] on 23 March 2025 at 10:51:17 am'!

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/23/2025 10:49:26'!
askIfImplementorShouldBeRemoved: aMessage

	shouldRemoveImplementor := PopUpMenu confirm: aMessage orCancel: [ self endRequest ]! !

