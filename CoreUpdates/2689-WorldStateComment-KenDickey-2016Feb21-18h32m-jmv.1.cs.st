'From Cuis 4.2 of 25 July 2013 [latest update: #2688] on 21 February 2016 at 6:36:27.463555 pm'!

!PasteUpMorph commentStamp: 'jmv 2/21/2016 18:32' prior: 0!
A World, the entire Smalltalk screen, is a PasteUpMorph.  A World responds true to isWorld.  A PasteUpMorph that is a world, builds its menu in HandMorph buildWorldMenu.

worldState		If I am also a World, keeps the hands, damageRecorder, stepList etc.
!


!WorldState commentStamp: 'jmv 2/21/2016 18:35' prior: 0!
The state of a Morphic world.

Used as the Model of a PasteUpMorph (World) Display.

hands 						-- Array of HandMorphs (typically only one) representing the Cursor and its event queue.
viewBox 					-- Physical screen size. (Display boundingBox)
canvas 					-- BitBlitCanvas on the DisplayScreen. (Display getCanvas)
damageRecorder 		-- the DamageRecorder for the Display
stepList 					-- a Heap of StepMessage.  Each morph which wants stepping has a StepMessage here.
								See comment in #Morph>>stepAt:
lastStepTime 			-- 'now' as last sampled at step. (Time localMillisecondClock)
lastStepMessage 		-- nil or a selector
lastCycleTime 			-- 'now' as last sampled (Time localMillisecondClock)
alarms 					-- a Heap of MorphicAlarm.  See comment in #Morph>>addAlarm:after:
								An _alarm_ is an action to be done once, vs a _step_ which is cycled.
lastAlarm 					-- 'now' as sampled at last alarm (Time localMillisecondClock).
remoteServer 			-- nil (??Unused??)
drawingFailureMorphs 	-- a WeakIdentitySet of Morphs with drawing failures
waitDelay 				-- a Delay (set in #WorldState>>doOneCycleFor:)
pause 						-- A 'phase-locked loop' style value to help regularise the step/alarm/event service rate.
		 						(set in #WorldState>>doOneCycleFor:)
lastCycleHadAnyEvent 	-- a boolean (set in #WorldState>>doOneCycleFor:)
activeHand 				-- a HandMorph
world 						-- a PasteUpMorph 
!

