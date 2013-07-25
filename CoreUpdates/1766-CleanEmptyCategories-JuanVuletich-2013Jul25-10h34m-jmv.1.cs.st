'From Cuis 4.2 of 25 July 2013 [latest update: #1765] on 25 July 2013 at 10:34:57.453 am'!

!InputSensor reorganize!
('buttons' buttons)
('cursor' currentCursor currentCursor:)
('initialize' flushEvents shutDown startUp)
('joystick' joystickButtons: joystickOn: joystickXY: testJoystick:)
('keyboard' flushKeyboard kbdTest keyboard keyboardPeek keyboardPressed)
('modifier keys' anyModifierKeyPressed commandAltKeyPressed controlKeyPressed macOptionKeyPressed rawMacOptionKeyPressed shiftPressed)
('mouse' isAnyButtonPressed isMouseButton1Pressed isMouseButton2Pressed isMouseButton3Pressed mouseButtons mousePoint mousePoint: noButtonPressed peekButtons peekMousePt waitButton waitButtonOrKeyboard waitClickButton waitNoButton)
('tablet' hasTablet tabletExtent tabletPoint tabletPressure tabletTimestamp)
('user interrupts' eventTicklerProcess installInterruptWatcher interruptWatcherProcess setInterruptKey: userInterruptWatcher)
('private' characterForKeycode: primCursorLocPut: primCursorLocPutAgain: primInterruptSemaphore: primKbdNext primKbdPeek primMouseButtons primMousePt primReadJoystick: primSetInterruptKey: primTabletGetParameters: primTabletRead:)
!


!Feature class reorganize!
('instance creation' baseSystemFeature name: name:version: name:version:revision:)
!

