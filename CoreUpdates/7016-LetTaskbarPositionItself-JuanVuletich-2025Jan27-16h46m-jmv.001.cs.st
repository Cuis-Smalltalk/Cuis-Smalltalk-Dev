'From Cuis7.3 [latest update: #7015] on 27 January 2025 at 4:46:40 pm'!

!WorldMorph methodsFor: 'taskbar' stamp: 'jmv 1/27/2025 16:46:34'!
showTaskbar

	taskbar ifNil: [
		taskbar := TaskbarMorph newRow.
		self addMorph: taskbar ]! !

