# Global data
  .data
  .balign 8# .align 2
  .type global_data_beginning, @object
global_data_beginning:
  #.section .data.rel
  .globl global_data_beginning
p0:
  .quad 0xbad # omitted from compilation
p2:
  .quad 1, 102
p5:
  .quad p18, 0
p6:
  .quad 5, 258038, p525, 0
p234:
  .quad 15, 105, 111, 58, 119, 114, 105, 116, 101, 45, 102, 105, 120, 110, 117, 109
p238:
  .quad 0, 1016, p67
p246:
  .quad 21, 115, 116, 114, 105, 110, 103, 58, 102, 105, 120, 110, 117, 109, 45, 62, 115, 116, 114, 105, 110, 103
p672:
  .quad 12, 102, 105, 120, 110, 117, 109, 58, 122, 101, 114, 111, 63
p678:
  .quad 14, 119, 104, 97, 116, 101, 118, 101, 114, 58, 122, 101, 114, 111, 63
p683:
  .quad 0, 1, p99
p682:
  .quad p683, 0
p680:
  .quad 3, 2, p675, p682
p675: # the symbol whatever:zero?
  .quad p678, 0, 127, p0, p680, 0, 0, p0, 0, whatever_58_zero_63_, 0
p677:
  .quad 0, 25, p99
p676:
  .quad p677, 0
p674:
  .quad 3, 26, p675, p676
p669: # the symbol fixnum:zero?
  .quad p672, 0, 127, p0, p674, 0, 0, 0, 0, fixnum_58_zero_63_, 0
p671:
  .quad 0, 883, p244
p670:
  .quad p671, 0
p249:
  .quad 5, 884, p669, p670
p250:
  .quad 0, 0
p666:
  .quad 0, 885, p244
p668:
  .quad 1, 886, 0
p667:
  .quad p668, 0
p665:
  .quad p666, p667
p254:
  .quad 5, 887, p481, p665
p255:
  .quad 0, 0
p664:
  .quad 0, 888, p244
p663:
  .quad p664, 0
p256:
  .quad 5, 889, p263, p663
p543:
  .quad 14, 115, 116, 114, 105, 110, 103, 58, 97, 112, 112, 101, 110, 100, 50
p556:
  .quad 14, 118, 101, 99, 116, 111, 114, 58, 97, 112, 112, 101, 110, 100, 50
p559:
  .quad p589, 0
p662:
  .quad 0, 748, p651
p661:
  .quad p662, 0
p560:
  .quad 5, 749, p40, p661
p562:
  .quad p583, 0
p660:
  .quad 0, 750, p585
p659:
  .quad p660, 0
p563:
  .quad 5, 751, p40, p659
p565:
  .quad p286, 0
p656:
  .quad 0, 752, p589
p658:
  .quad 0, 753, p583
p657:
  .quad p658, 0
p655:
  .quad p656, p657
p654:
  .quad 5, 754, p188, p655
p653:
  .quad p654, 0
p566:
  .quad 5, 755, p411, p653
p642:
  .quad 0, 756, p286
p644:
  .quad 1, 757, 1
p652:
  .quad 2, 118, 49
p651: # the symbol v1
  .quad p652, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p646:
  .quad 0, 758, p651
p648:
  .quad 1, 759, 1
p650:
  .quad 0, 760, p589
p649:
  .quad p650, 0
p647:
  .quad p648, p649
p645:
  .quad p646, p647
p643:
  .quad p644, p645
p641:
  .quad p642, p643
p568:
  .quad 5, 761, p572, p641
p591:
  .quad 23, 118, 101, 99, 116, 111, 114, 58, 98, 108, 105, 116, 45, 102, 114, 111, 109, 45, 104, 101, 97, 100, 101, 114
p594:
  .quad 0, 775, p615
p595:
  .quad 0, 0
p596:
  .quad 2, 776, 0
p598:
  .quad p635, 0
p638:
  .quad 0, 777, p621
p640:
  .quad 0, 778, p619
p639:
  .quad p640, 0
p637:
  .quad p638, p639
p599:
  .quad 5, 779, p48, p637
p630:
  .quad 0, 780, p627
p632:
  .quad 0, 781, p625
p636:
  .quad 4, 119, 111, 114, 100
p635: # the symbol word
  .quad p636, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p634:
  .quad 0, 782, p635
p633:
  .quad p634, 0
p631:
  .quad p632, p633
p629:
  .quad p630, p631
p601:
  .quad 5, 783, p394, p629
p628:
  .quad 6, 116, 97, 114, 103, 101, 116
p627: # the symbol target
  .quad p628, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p604:
  .quad 0, 784, p627
p626:
  .quad 24, 116, 97, 114, 103, 101, 116, 45, 105, 110, 100, 101, 120, 45, 102, 114, 111, 109, 45, 104, 101, 97, 100, 101, 114
p625: # the symbol target-index-from-header
  .quad p626, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p624:
  .quad 0, 785, p625
p623:
  .quad p624, 0
p606:
  .quad 5, 786, p88, p623
p622:
  .quad 6, 115, 111, 117, 114, 99, 101
p621: # the symbol source
  .quad p622, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p608:
  .quad 0, 787, p621
p620:
  .quad 24, 115, 111, 117, 114, 99, 101, 45, 105, 110, 100, 101, 120, 45, 102, 114, 111, 109, 45, 104, 101, 97, 100, 101, 114
p619: # the symbol source-index-from-header
  .quad p620, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p618:
  .quad 0, 788, p619
p617:
  .quad p618, 0
p610:
  .quad 5, 789, p88, p617
p616:
  .quad 7, 119, 111, 114, 100, 45, 110, 111
p615: # the symbol word-no
  .quad p616, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p614:
  .quad 0, 790, p615
p613:
  .quad p614, 0
p612:
  .quad 5, 791, p195, p613
p611:
  .quad p612, 0
p609:
  .quad p610, p611
p607:
  .quad p608, p609
p605:
  .quad p606, p607
p603:
  .quad p604, p605
p602:
  .quad 5, 792, p572, p603
p600:
  .quad 4, 793, 0, p601, p602
p597:
  .quad 4, 794, p598, p599, p600
p593:
  .quad 7, 795, p594, p595, p596, p597
p572: # the symbol vector:blit-from-header
  .quad p591, 0, 127, p0, p593, 0, 0, 0, 0, vector_58_blit_45_from_45_header, 0
p574:
  .quad 0, 762, p286
p590:
  .quad 7, 108, 101, 110, 103, 116, 104, 49
p589: # the symbol length1
  .quad p590, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p588:
  .quad 0, 763, p589
p587:
  .quad p588, 0
p576:
  .quad 5, 764, p88, p587
p586:
  .quad 2, 118, 50
p585: # the symbol v2
  .quad p586, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p578:
  .quad 0, 765, p585
p580:
  .quad 1, 766, 1
p584:
  .quad 7, 108, 101, 110, 103, 116, 104, 50
p583: # the symbol length2
  .quad p584, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p582:
  .quad 0, 767, p583
p581:
  .quad p582, 0
p579:
  .quad p580, p581
p577:
  .quad p578, p579
p575:
  .quad p576, p577
p573:
  .quad p574, p575
p570:
  .quad 5, 768, p572, p573
p571:
  .quad 0, 769, p286
p569:
  .quad 4, 770, 0, p570, p571
p567:
  .quad 4, 771, 0, p568, p569
p564:
  .quad 4, 772, p565, p566, p567
p561:
  .quad 4, 773, p562, p563, p564
p558:
  .quad 4, 774, p559, p560, p561
p546: # the symbol vector:append2
  .quad p556, 0, 127, p0, p558, 0, 0, 0, 0, vector_58_append2, 0
p554:
  .quad 2, 115, 49
p555:
  .quad 546, 39, 97, 32, 32, 32, 32, 32, 59, 59, 32, 108, 105, 110, 101, 32, 49, 10, 40, 97, 41, 32, 32, 32, 32, 32, 59, 59, 32, 108, 105, 110, 101, 32, 50, 10, 40, 97, 32, 46, 32, 98, 41, 32, 59, 59, 32, 108, 105, 110, 101, 32, 51, 10, 97, 98, 99, 32, 32, 32, 32, 32, 59, 59, 32, 108, 105, 110, 101, 32, 52, 10, 40, 97, 41, 32, 32, 32, 32, 32, 59, 59, 32, 108, 105, 110, 101, 32, 53, 10, 40, 97, 32, 46, 32, 98, 41, 32, 59, 59, 32, 108, 105, 110, 101, 32, 54, 10, 40, 97, 32, 98, 41, 32, 32, 32, 59, 59, 32, 108, 105, 110, 101, 32, 55, 10, 34, 97, 98, 99, 100, 34, 10, 96, 40, 40, 97, 32, 46, 32, 98, 41, 32, 44, 99, 32, 40, 40, 97, 32, 40, 100, 32, 101, 32, 46, 32, 102, 41, 41, 32, 104, 41, 32, 44, 64, 103, 41, 10, 35, 116, 59, 59, 32, 97, 32, 99, 111, 109, 109, 101, 110, 116, 32, 114, 105, 103, 104, 116, 32, 97, 102, 116, 101, 114, 32, 116, 104, 101, 32, 97, 116, 111, 109, 10, 45, 10, 97, 45, 98, 10, 39, 113, 117, 111, 116, 101, 100, 10, 96, 113, 117, 97, 115, 105, 113, 117, 111, 116, 101, 100, 10, 44, 117, 110, 113, 117, 111, 116, 101, 100, 10, 44, 64, 117, 110, 113, 117, 111, 116, 101, 100, 45, 115, 112, 108, 105, 99, 105, 110, 103, 10, 35, 59, 40, 97, 32, 40, 118, 101, 114, 121, 41, 32, 99, 111, 109, 112, 108, 101, 120, 32, 46, 32, 99, 111, 109, 109, 101, 110, 116, 41, 32, 52, 53, 50, 10, 40, 32, 41, 10, 40, 41, 10, 35, 102, 10, 40, 97, 32, 98, 41, 10, 40, 97, 32, 46, 32, 98, 41, 10, 40, 46, 32, 98, 41, 10, 40, 100, 101, 102, 105, 110, 101, 32, 40, 102, 97, 99, 116, 32, 110, 41, 10, 32, 32, 40, 105, 102, 32, 40, 122, 101, 114, 111, 63, 32, 110, 41, 10, 32, 32, 32, 32, 32, 49, 10, 32, 32, 32, 32, 32, 40, 42, 32, 110, 32, 40, 102, 97, 99, 116, 32, 40, 49, 45, 32, 110, 41, 41, 41, 41, 41, 10, 34, 116, 104, 105, 115, 32, 105, 115, 32, 97, 32, 115, 116, 114, 105, 110, 103, 34, 10, 40, 100, 101, 102, 105, 110, 101, 32, 40, 102, 97, 99, 116, 32, 110, 41, 10, 32, 32, 40, 105, 102, 32, 40, 122, 101, 114, 111, 63, 32, 110, 41, 10, 32, 32, 32, 32, 32, 49, 10, 32, 32, 32, 32, 32, 40, 42, 32, 110, 32, 40, 102, 97, 99, 116, 32, 40, 49, 45, 32, 110, 41, 41, 41, 41, 41, 10, 40, 100, 101, 102, 105, 110, 101, 32, 40, 102, 97, 99, 116, 32, 110, 41, 10, 32, 32, 40, 105, 102, 32, 40, 122, 101, 114, 111, 63, 32, 110, 41, 10, 32, 32, 32, 32, 32, 49, 10, 32, 32, 32, 32, 32, 40, 42, 32, 110, 32, 40, 102, 97, 99, 116, 32, 40, 49, 45, 32, 110, 41, 41, 41, 41, 41, 10
p553: # the symbol s1
  .quad p554, 1, p555, 0, 0, 0, 0, 0, 0, 0, 0
p548:
  .quad 0, 865, p553
p552:
  .quad 2, 115, 50
p551: # the symbol s2
  .quad p552, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p550:
  .quad 0, 866, p551
p549:
  .quad p550, 0
p547:
  .quad p548, p549
p545:
  .quad 5, 867, p546, p547
p258: # the symbol string:append2
  .quad p543, 0, 127, p0, p545, 0, 0, 0, 0, string_58_append2, 0
p542:
  .quad 1, 45
p260:
  .quad 1, 890, p542
p275:
  .quad 30, 115, 116, 114, 105, 110, 103, 58, 112, 111, 115, 105, 116, 105, 118, 101, 45, 102, 105, 120, 110, 117, 109, 45, 62, 115, 116, 114, 105, 110, 103
p278:
  .quad p295, 0
p441:
  .quad 0, 898, p244
p442:
  .quad 0, 0
p443:
  .quad 1, 899, 1
p450:
  .quad 12, 102, 105, 120, 110, 117, 109, 58, 108, 111, 103, 49, 48
p453:
  .quad 0, 169, p198
p454:
  .quad 0, 0
p498:
  .quad 8, 101, 49, 58, 101, 114, 114, 111, 114
p541:
  .quad 7, 69, 82, 82, 79, 82, 58, 32
p540:
  .quad 1, 4383, p541
p539:
  .quad p540, 0
p501:
  .quad 5, 4384, p529, p539
p532:
  .quad 12, 115, 116, 114, 105, 110, 103, 58, 119, 114, 105, 116, 101
p536:
  .quad 5, 857, p525, 0
p538:
  .quad 0, 858, p43
p537:
  .quad p538, 0
p535:
  .quad p536, p537
p534:
  .quad 5, 859, p12, p535
p529: # the symbol string:write
  .quad p532, 0, 127, p0, p534, 0, 0, 0, 0, string_58_write, 0
p531:
  .quad 0, 4385, p43
p530:
  .quad p531, 0
p503:
  .quad 5, 4386, p529, p530
p518:
  .quad 15, 99, 104, 97, 114, 97, 99, 116, 101, 114, 58, 119, 114, 105, 116, 101
p526:
  .quad 18, 105, 111, 58, 115, 116, 97, 110, 100, 97, 114, 100, 45, 111, 117, 116, 112, 117, 116
p527:
  .quad 3, 948, p525, 0
p525: # the symbol io:standard-output
  .quad p526, 0, 127, 0, p527, 0, 0, p0, 0, io_58_standard_45_output, 0
p522:
  .quad 5, 846, p525, 0
p524:
  .quad 0, 847, p148
p523:
  .quad p524, 0
p521:
  .quad p522, p523
p520:
  .quad 3, 848, p101, p521
p513: # the symbol character:write
  .quad p518, 0, 127, p0, p520, 0, 0, 0, 0, character_58_write, 0
p517:
  .quad 17, 99, 104, 97, 114, 97, 99, 116, 101, 114, 58, 110, 101, 119, 108, 105, 110, 101
p516: # the symbol character:newline
  .quad p517, 1, 10, 0, 0, 0, 0, 0, 0, 0, 0
p515:
  .quad 0, 4387, p516
p514:
  .quad p515, 0
p505:
  .quad 5, 4388, p513, p514
p508:
  .quad 7, 101, 49, 58, 102, 97, 105, 108
p511:
  .quad 10, 100, 101, 98, 117, 103, 58, 102, 97, 105, 108
p510: # the symbol debug:fail
  .quad p511, 0, 127, 0, 0, 0, 0, p0, 0, 0, 0
p509:
  .quad 3, 4382, p510, 0
p507: # the symbol e1:fail
  .quad p508, 0, 127, 0, p509, 0, 0, 0, 0, e1_58_fail, 0
p506:
  .quad 5, 4389, p507, 0
p504:
  .quad 4, 4390, 0, p505, p506
p502:
  .quad 4, 4391, 0, p503, p504
p500:
  .quad 4, 4392, 0, p501, p502
p494: # the symbol e1:error
  .quad p498, 0, 127, p0, p500, 0, 0, 0, 0, e1_58_error, 0
p497:
  .quad 5, 114, 97, 110, 103, 101
p496:
  .quad 1, 170, p497
p495:
  .quad p496, 0
p455:
  .quad 5, 171, p494, p495
p462:
  .quad 19, 102, 105, 120, 110, 117, 109, 58, 108, 111, 103, 49, 48, 45, 104, 101, 108, 112, 101, 114
p486:
  .quad 8, 102, 105, 120, 110, 117, 109, 58, 60
p491:
  .quad 0, 55, p99
p493:
  .quad 0, 56, p163
p492:
  .quad p493, 0
p490:
  .quad p491, p492
p488:
  .quad 3, 57, p481, p490
p481: # the symbol fixnum:<
  .quad p486, 0, 127, p0, p488, 0, 0, p0, 0, fixnum_58__60_, 0
p483:
  .quad 0, 176, p198
p485:
  .quad 1, 177, 10
p484:
  .quad p485, 0
p482:
  .quad p483, p484
p465:
  .quad 5, 178, p481, p482
p466:
  .quad 0, 0
p478:
  .quad 0, 179, p198
p480:
  .quad 1, 180, 10
p479:
  .quad p480, 0
p477:
  .quad p478, p479
p472:
  .quad 5, 181, p316, p477
p476:
  .quad 0, 182, p469
p475:
  .quad p476, 0
p474:
  .quad 5, 183, p88, p475
p473:
  .quad p474, 0
p471:
  .quad p472, p473
p467:
  .quad 5, 184, p457, p471
p470:
  .quad 3, 97, 99, 99
p469: # the symbol acc
  .quad p470, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p468:
  .quad 0, 185, p469
p464:
  .quad 7, 186, p465, p466, p467, p468
p457: # the symbol fixnum:log10-helper
  .quad p462, 0, 127, p0, p464, 0, 0, 0, 0, fixnum_58_log10_45_helper, 0
p459:
  .quad 0, 172, p198
p461:
  .quad 1, 173, 0
p460:
  .quad p461, 0
p458:
  .quad p459, p460
p456:
  .quad 5, 174, p457, p458
p452:
  .quad 7, 175, p453, p454, p455, p456
p447: # the symbol fixnum:log10
  .quad p450, 0, 127, p0, p452, 0, 0, 0, 0, fixnum_58_log10, 0
p449:
  .quad 0, 900, p244
p448:
  .quad p449, 0
p446:
  .quad 5, 901, p447, p448
p445:
  .quad p446, 0
p444:
  .quad 5, 902, p88, p445
p279:
  .quad 7, 903, p441, p442, p443, p444
p281:
  .quad p286, 0
p414:
  .quad 11, 118, 101, 99, 116, 111, 114, 58, 109, 97, 107, 101
p417:
  .quad p286, 0
p435:
  .quad 11, 98, 117, 102, 102, 101, 114, 58, 109, 97, 107, 101
p440:
  .quad 0, 187, p428
p439:
  .quad p440, 0
p437:
  .quad 3, 188, p430, p439
p430: # the symbol buffer:make
  .quad p435, 0, 127, p0, p437, 0, 0, p0, 0, buffer_58_make, 0
p434:
  .quad 0, 623, p428
p433:
  .quad p434, 0
p432:
  .quad 5, 624, p88, p433
p431:
  .quad p432, 0
p418:
  .quad 5, 625, p430, p431
p423:
  .quad 0, 626, p286
p425:
  .quad 1, 627, 0
p429:
  .quad 10, 101, 108, 101, 109, 101, 110, 116, 45, 110, 111
p428: # the symbol element-no
  .quad p429, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p427:
  .quad 0, 628, p428
p426:
  .quad p427, 0
p424:
  .quad p425, p426
p422:
  .quad p423, p424
p420:
  .quad 5, 629, p394, p422
p421:
  .quad 0, 630, p286
p419:
  .quad 4, 631, 0, p420, p421
p416:
  .quad 4, 632, p417, p418, p419
p411: # the symbol vector:make
  .quad p414, 0, 127, p0, p416, 0, 0, 0, 0, vector_58_make, 0
p413:
  .quad 0, 904, p295
p412:
  .quad p413, 0
p282:
  .quad 5, 905, p411, p412
p297:
  .quad 40, 115, 116, 114, 105, 110, 103, 58, 102, 105, 108, 108, 45, 119, 105, 116, 104, 45, 112, 111, 115, 105, 116, 105, 118, 101, 45, 102, 105, 120, 110, 117, 109, 45, 100, 105, 103, 105, 116, 115, 33
p300:
  .quad 0, 914, p314
p301:
  .quad 0, 0
p302:
  .quad 2, 915, 0
p365:
  .quad 11, 115, 116, 114, 105, 110, 103, 58, 115, 101, 116, 33
p377:
  .quad 11, 118, 101, 99, 116, 111, 114, 58, 115, 101, 116, 33
p391:
  .quad 23, 118, 101, 99, 116, 111, 114, 58, 115, 101, 116, 45, 102, 114, 111, 109, 45, 104, 101, 97, 100, 101, 114, 33
p401:
  .quad 11, 98, 117, 102, 102, 101, 114, 58, 115, 101, 116, 33
p406:
  .quad 0, 194, p65
p408:
  .quad 0, 195, p63
p410:
  .quad 0, 196, p387
p409:
  .quad p410, 0
p407:
  .quad p408, p409
p405:
  .quad p406, p407
p403:
  .quad 3, 197, p394, p405
p394: # the symbol buffer:set!
  .quad p401, 0, 127, p0, p403, 0, 0, p0, 0, buffer_58_set_33_, 0
p396:
  .quad 0, 667, p53
p398:
  .quad 0, 668, p138
p400:
  .quad 0, 669, p387
p399:
  .quad p400, 0
p397:
  .quad p398, p399
p395:
  .quad p396, p397
p393:
  .quad 5, 670, p394, p395
p380: # the symbol vector:set-from-header!
  .quad p391, 0, 127, p0, p393, 0, 0, 0, 0, vector_58_set_45_from_45_header_33_, 0
p382:
  .quad 0, 675, p53
p390:
  .quad 0, 676, p63
p389:
  .quad p390, 0
p384:
  .quad 5, 677, p88, p389
p388:
  .quad 11, 110, 101, 119, 45, 101, 108, 101, 109, 101, 110, 116
p387: # the symbol new-element
  .quad p388, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p386:
  .quad 0, 678, p387
p385:
  .quad p386, 0
p383:
  .quad p384, p385
p381:
  .quad p382, p383
p379:
  .quad 5, 679, p380, p381
p368: # the symbol vector:set!
  .quad p377, 0, 127, p0, p379, 0, 0, 0, 0, vector_58_set_33_, 0
p370:
  .quad 0, 876, p35
p372:
  .quad 0, 877, p119
p376:
  .quad 1, 99
p375: # the symbol c
  .quad p376, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p374:
  .quad 0, 878, p375
p373:
  .quad p374, 0
p371:
  .quad p372, p373
p369:
  .quad p370, p371
p367:
  .quad 5, 879, p368, p369
p331: # the symbol string:set!
  .quad p365, 0, 127, p0, p367, 0, 0, 0, 0, string_58_set_33_, 0
p333:
  .quad 0, 916, p43
p364:
  .quad 0, 917, p314
p363:
  .quad p364, 0
p335:
  .quad 5, 918, p195, p363
p354:
  .quad 27, 99, 104, 97, 114, 97, 99, 116, 101, 114, 58, 102, 105, 120, 110, 117, 109, 45, 62, 99, 104, 97, 114, 97, 99, 116, 101, 114
p358:
  .quad 0, 851, p244
p362:
  .quad 24, 99, 104, 97, 114, 97, 99, 116, 101, 114, 58, 48, 45, 97, 115, 45, 99, 104, 97, 114, 97, 99, 116, 101, 114
p361: # the symbol character:0-as-character
  .quad p362, 1, 48, 0, 0, 0, 0, 0, 0, 0, 0
p360:
  .quad 0, 852, p361
p359:
  .quad p360, 0
p357:
  .quad p358, p359
p356:
  .quad 5, 853, p188, p357
p338: # the symbol character:fixnum->character
  .quad p354, 0, 127, p0, p356, 0, 0, 0, 0, character_58_fixnum_45__62_character, 0
p346:
  .quad 8, 102, 105, 120, 110, 117, 109, 58, 37
p351:
  .quad 0, 45, p99
p353:
  .quad 0, 46, p163
p352:
  .quad p353, 0
p350:
  .quad p351, p352
p348:
  .quad 3, 47, p341, p350
p341: # the symbol fixnum:%
  .quad p346, 0, 127, p0, p348, 0, 0, p0, 0, fixnum_58__37_, 0
p343:
  .quad 0, 919, p244
p345:
  .quad 1, 920, 10
p344:
  .quad p345, 0
p342:
  .quad p343, p344
p340:
  .quad 5, 921, p341, p342
p339:
  .quad p340, 0
p337:
  .quad 5, 922, p338, p339
p336:
  .quad p337, 0
p334:
  .quad p335, p336
p332:
  .quad p333, p334
p304:
  .quad 5, 923, p331, p332
p307:
  .quad 0, 924, p43
p321:
  .quad 8, 102, 105, 120, 110, 117, 109, 58, 47
p328:
  .quad 0, 42, p99
p330:
  .quad 0, 43, p163
p329:
  .quad p330, 0
p327:
  .quad p328, p329
p323:
  .quad 3, 44, p316, p327
p316: # the symbol fixnum:/
  .quad p321, 0, 127, p0, p323, p0, p0, p0, 0, fixnum_58__47_, 0
p318:
  .quad 0, 925, p244
p320:
  .quad 1, 926, 10
p319:
  .quad p320, 0
p317:
  .quad p318, p319
p309:
  .quad 5, 927, p316, p317
p315:
  .quad 18, 114, 101, 109, 97, 105, 110, 105, 110, 103, 45, 100, 105, 103, 105, 116, 45, 110, 111
p314: # the symbol remaining-digit-no
  .quad p315, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p313:
  .quad 0, 928, p314
p312:
  .quad p313, 0
p311:
  .quad 5, 929, p195, p312
p310:
  .quad p311, 0
p308:
  .quad p309, p310
p306:
  .quad p307, p308
p305:
  .quad 5, 930, p288, p306
p303:
  .quad 4, 931, 0, p304, p305
p299:
  .quad 7, 932, p300, p301, p302, p303
p288: # the symbol string:fill-with-positive-fixnum-digits!
  .quad p297, 0, 127, p0, p299, 0, 0, 0, 0, string_58_fill_45_with_45_positive_45_fixnum_45_digits_33_, 0
p290:
  .quad 0, 906, p286
p292:
  .quad 0, 907, p244
p296:
  .quad 8, 100, 105, 103, 105, 116, 45, 110, 111
p295: # the symbol digit-no
  .quad p296, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p294:
  .quad 0, 908, p295
p293:
  .quad p294, 0
p291:
  .quad p292, p293
p289:
  .quad p290, p291
p284:
  .quad 5, 909, p288, p289
p287:
  .quad 6, 114, 101, 115, 117, 108, 116
p286: # the symbol result
  .quad p287, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p285:
  .quad 0, 910, p286
p283:
  .quad 4, 911, 0, p284, p285
p280:
  .quad 4, 912, p281, p282, p283
p277:
  .quad 4, 913, p278, p279, p280
p263: # the symbol string:positive-fixnum->string
  .quad p275, 0, 127, p0, p277, 0, 0, 0, 0, string_58_positive_45_fixnum_45__62_string, 0
p269:
  .quad 13, 102, 105, 120, 110, 117, 109, 58, 110, 101, 103, 97, 116, 101
p274:
  .quad 0, 34, p99
p273:
  .quad p274, 0
p271:
  .quad 3, 35, p266, p273
p266: # the symbol fixnum:negate
  .quad p269, 0, 127, p0, p271, 0, 0, p0, 0, fixnum_58_negate, 0
p268:
  .quad 0, 891, p244
p267:
  .quad p268, 0
p265:
  .quad 5, 892, p266, p267
p264:
  .quad p265, 0
p262:
  .quad 5, 893, p263, p264
p261:
  .quad p262, 0
p259:
  .quad p260, p261
p257:
  .quad 5, 894, p258, p259
p251:
  .quad 7, 895, p254, p255, p256, p257
p253:
  .quad 1, 48
p252:
  .quad 1, 896, p253
p248:
  .quad 7, 897, p249, p250, p251, p252
p241: # the symbol string:fixnum->string
  .quad p246, 0, 127, p0, p248, 0, 0, 0, 0, string_58_fixnum_45__62_string, 0
p245:
  .quad 6, 102, 105, 120, 110, 117, 109
p244: # the symbol fixnum
  .quad p245, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p243:
  .quad 0, 1017, p244
p242:
  .quad p243, 0
p240:
  .quad 5, 1018, p241, p242
p239:
  .quad p240, 0
p237:
  .quad p238, p239
p236:
  .quad 5, 1019, p12, p237
p173: # the symbol io:write-fixnum
  .quad p234, 0, 127, p0, p236, 0, 0, 0, 0, io_58_write_45_fixnum, 0
p175:
  .quad 0, 258039, p18
p181:
  .quad 4, 102, 105, 98, 111
p184:
  .quad 0, 78519, p198
p233:
  .quad 1, 0
p185:
  .quad 0, p233
p186:
  .quad 0, 78520, p198
p223:
  .quad 8, 102, 105, 120, 110, 117, 109, 58, 43
p230:
  .quad 0, 31, p99
p232:
  .quad 0, 32, p163
p231:
  .quad p232, 0
p229:
  .quad p230, p231
p225:
  .quad 3, 33, p188, p229
p188: # the symbol fixnum:+
  .quad p223, 0, 127, p0, p225, p0, p0, p0, 0, fixnum_58__43_, 0
p213:
  .quad 8, 102, 105, 120, 110, 117, 109, 58, 45
p220:
  .quad 0, 36, p99
p222:
  .quad 0, 37, p163
p221:
  .quad p222, 0
p219:
  .quad p220, p221
p215:
  .quad 3, 38, p208, p219
p208: # the symbol fixnum:-
  .quad p213, 0, 127, p0, p215, p0, p0, p0, 0, fixnum_58__45_, 0
p210:
  .quad 0, 78521, p198
p212:
  .quad 1, 78522, 2
p211:
  .quad p212, 0
p209:
  .quad p210, p211
p207:
  .quad 5, 78523, p208, p209
p206:
  .quad p207, 0
p190:
  .quad 5, 78524, p178, p206
p200:
  .quad 9, 102, 105, 120, 110, 117, 109, 58, 49, 45
p205:
  .quad 0, 29, p99
p204:
  .quad p205, 0
p202:
  .quad 3, 30, p195, p204
p195: # the symbol fixnum:1-
  .quad p200, 0, 127, p0, p202, 0, 0, p0, 0, fixnum_58_1_45_, 0
p199:
  .quad 1, 110
p198: # the symbol n
  .quad p199, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p197:
  .quad 0, 78525, p198
p196:
  .quad p197, 0
p194:
  .quad 5, 78526, p195, p196
p193:
  .quad p194, 0
p192:
  .quad 5, 78527, p178, p193
p191:
  .quad p192, 0
p189:
  .quad p190, p191
p187:
  .quad 5, 78528, p188, p189
p183:
  .quad 7, 78529, p184, p185, p186, p187
p178: # the symbol fibo
  .quad p181, 0, 127, p0, p183, 0, 0, 0, 0, fibo, 0
p180:
  .quad 1, 258040, 34
p179:
  .quad p180, 0
p177:
  .quad 5, 258041, p178, p179
p176:
  .quad p177, 0
p174:
  .quad p175, p176
p8:
  .quad 5, 258042, p173, p174
p20:
  .quad 15, 105, 111, 58, 119, 114, 105, 116, 101, 45, 115, 116, 114, 105, 110, 103
p69:
  .quad 20, 105, 111, 58, 119, 114, 105, 116, 101, 45, 115, 116, 114, 105, 110, 103, 45, 102, 114, 111, 109
p155:
  .quad 8, 102, 105, 120, 110, 117, 109, 58, 61
p165:
  .quad 12, 119, 104, 97, 116, 101, 118, 101, 114, 58, 101, 113, 63
p170:
  .quad 0, 3, p99
p172:
  .quad 0, 4, p163
p171:
  .quad p172, 0
p169:
  .quad p170, p171
p167:
  .quad 3, 5, p158, p169
p158: # the symbol whatever:eq?
  .quad p165, 0, 127, p0, p167, 0, 0, p0, 0, whatever_58_eq_63_, 0
p160:
  .quad 0, 48, p99
p164:
  .quad 1, 98
p163: # the symbol b
  .quad p164, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p162:
  .quad 0, 49, p163
p161:
  .quad p162, 0
p159:
  .quad p160, p161
p157:
  .quad 3, 50, p158, p159
p150: # the symbol fixnum:=
  .quad p155, 0, 127, p0, p157, 0, 0, 0, 0, fixnum_58__61_, 0
p152:
  .quad 0, 973, p91
p154:
  .quad 0, 974, p86
p153:
  .quad p154, 0
p151:
  .quad p152, p153
p72:
  .quad 5, 975, p150, p151
p73:
  .quad 0, 0
p140:
  .quad 18, 105, 111, 58, 119, 114, 105, 116, 101, 45, 99, 104, 97, 114, 97, 99, 116, 101, 114
p145:
  .quad 0, 959, p67
p149:
  .quad 9, 99, 104, 97, 114, 97, 99, 116, 101, 114
p148: # the symbol character
  .quad p149, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p147:
  .quad 0, 960, p148
p146:
  .quad p147, 0
p144:
  .quad p145, p146
p142:
  .quad 3, 961, p101, p144
p101: # the symbol io:write-character
  .quad p140, 0, 127, p0, p142, 0, 0, p0, 0, io_58_write_45_character, 0
p103:
  .quad 0, 976, p67
p111:
  .quad 10, 115, 116, 114, 105, 110, 103, 58, 103, 101, 116
p121:
  .quad 10, 118, 101, 99, 116, 111, 114, 58, 103, 101, 116
p131:
  .quad 22, 118, 101, 99, 116, 111, 114, 58, 103, 101, 116, 45, 102, 114, 111, 109, 45, 104, 101, 97, 100, 101, 114
p135:
  .quad 0, 664, p53
p139:
  .quad 18, 104, 101, 97, 100, 101, 114, 45, 98, 97, 115, 101, 100, 45, 105, 110, 100, 101, 120
p138: # the symbol header-based-index
  .quad p139, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p137:
  .quad 0, 665, p138
p136:
  .quad p137, 0
p134:
  .quad p135, p136
p133:
  .quad 5, 666, p48, p134
p124: # the symbol vector:get-from-header
  .quad p131, 0, 127, p0, p133, 0, 0, 0, 0, vector_58_get_45_from_45_header, 0
p126:
  .quad 0, 671, p53
p130:
  .quad 0, 672, p63
p129:
  .quad p130, 0
p128:
  .quad 5, 673, p88, p129
p127:
  .quad p128, 0
p125:
  .quad p126, p127
p123:
  .quad 5, 674, p124, p125
p114: # the symbol vector:get
  .quad p121, 0, 127, p0, p123, 0, 0, 0, 0, vector_58_get, 0
p116:
  .quad 0, 873, p35
p120:
  .quad 1, 105
p119: # the symbol i
  .quad p120, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p118:
  .quad 0, 874, p119
p117:
  .quad p118, 0
p115:
  .quad p116, p117
p113:
  .quad 5, 875, p114, p115
p106: # the symbol string:get
  .quad p111, 0, 127, p0, p113, 0, 0, 0, 0, string_58_get, 0
p108:
  .quad 0, 977, p35
p110:
  .quad 0, 978, p91
p109:
  .quad p110, 0
p107:
  .quad p108, p109
p105:
  .quad 5, 979, p106, p107
p104:
  .quad p105, 0
p102:
  .quad p103, p104
p76:
  .quad 5, 980, p101, p102
p79:
  .quad 0, 981, p67
p81:
  .quad 0, 982, p35
p93:
  .quad 9, 102, 105, 120, 110, 117, 109, 58, 49, 43
p100:
  .quad 1, 97
p99: # the symbol a
  .quad p100, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p98:
  .quad 0, 27, p99
p97:
  .quad p98, 0
p95:
  .quad 3, 28, p88, p97
p88: # the symbol fixnum:1+
  .quad p93, 0, 127, p0, p95, 0, 0, p0, 0, fixnum_58_1_43_, 0
p92:
  .quad 10, 102, 114, 111, 109, 45, 105, 110, 100, 101, 120
p91: # the symbol from-index
  .quad p92, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p90:
  .quad 0, 983, p91
p89:
  .quad p90, 0
p83:
  .quad 5, 984, p88, p89
p87:
  .quad 6, 108, 101, 110, 103, 116, 104
p86: # the symbol length
  .quad p87, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p85:
  .quad 0, 985, p86
p84:
  .quad p85, 0
p82:
  .quad p83, p84
p80:
  .quad p81, p82
p78:
  .quad p79, p80
p77:
  .quad 5, 986, p23, p78
p74:
  .quad 4, 987, 0, p76, p77
p75:
  .quad 2, 988, 0
p71:
  .quad 7, 989, p72, p73, p74, p75
p23: # the symbol io:write-string-from
  .quad p69, 0, 127, p0, p71, 0, 0, 0, 0, io_58_write_45_string_45_from, 0
p68:
  .quad 4, 102, 105, 108, 101
p67: # the symbol file
  .quad p68, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p25:
  .quad 0, 967, p67
p27:
  .quad 0, 968, p35
p29:
  .quad 1, 969, 0
p37:
  .quad 13, 115, 116, 114, 105, 110, 103, 58, 108, 101, 110, 103, 116, 104
p45:
  .quad 13, 118, 101, 99, 116, 111, 114, 58, 108, 101, 110, 103, 116, 104
p55:
  .quad 10, 98, 117, 102, 102, 101, 114, 58, 103, 101, 116
p66:
  .quad 6, 98, 117, 102, 102, 101, 114
p65: # the symbol buffer
  .quad p66, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p60:
  .quad 0, 191, p65
p64:
  .quad 5, 105, 110, 100, 101, 120
p63: # the symbol index
  .quad p64, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p62:
  .quad 0, 192, p63
p61:
  .quad p62, 0
p59:
  .quad p60, p61
p57:
  .quad 3, 193, p48, p59
p48: # the symbol buffer:get
  .quad p55, 0, 127, p0, p57, 0, 0, p0, 0, buffer_58_get, 0
p54:
  .quad 6, 118, 101, 99, 116, 111, 114
p53: # the symbol vector
  .quad p54, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p50:
  .quad 0, 661, p53
p52:
  .quad 1, 662, 0
p51:
  .quad p52, 0
p49:
  .quad p50, p51
p47:
  .quad 5, 663, p48, p49
p40: # the symbol vector:length
  .quad p45, 0, 127, p0, p47, 0, 0, 0, 0, vector_58_length, 0
p44:
  .quad 6, 115, 116, 114, 105, 110, 103
p43: # the symbol string
  .quad p44, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p42:
  .quad 0, 860, p43
p41:
  .quad p42, 0
p39:
  .quad 5, 861, p40, p41
p32: # the symbol string:length
  .quad p37, 0, 127, p0, p39, 0, 0, 0, 0, string_58_length, 0
p36:
  .quad 1, 115
p35: # the symbol s
  .quad p36, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p34:
  .quad 0, 970, p35
p33:
  .quad p34, 0
p31:
  .quad 5, 971, p32, p33
p30:
  .quad p31, 0
p28:
  .quad p29, p30
p26:
  .quad p27, p28
p24:
  .quad p25, p26
p22:
  .quad 5, 972, p23, p24
p12: # the symbol io:write-string
  .quad p20, 0, 127, p0, p22, 0, 0, 0, 0, io_58_write_45_string, 0
p19:
  .quad 5, 95, 50, 52, 55, 48
p18: # the symbol _2470
  .quad p19, 0, 127, 0, 0, 0, 0, 0, 0, 0, 0
p14:
  .quad 0, 258043, p18
p17:
  .quad 1, 10
p16:
  .quad 1, 258044, p17
p15:
  .quad p16, 0
p13:
  .quad p14, p15
p10:
  .quad 5, 258045, p12, p13
p11:
  .quad 2, 258046, 0
p9:
  .quad 4, 258047, 0, p10, p11
p7:
  .quad 4, 258048, 0, p8, p9
p4:
  .quad 4, 258049, p5, p6, p7
p1: # the symbol f
  .quad p2, 0, 127, 0, p4, 0, 0, 0, 0, f, 0
global_data_end:
  #.section .data.rel
  .globl global_data_end
  .quad 0


# Procedures
  .text

######### whatever:zero? (mangled as "whatever_58_zero_63_")
# io-no is 1
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl whatever_58_zero_63_
  .type whatever_58_zero_63_, @function
whatever_58_zero_63_:
  #[...]
whatever_58_zero_63__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 16(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*0(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $16, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size whatever_58_zero_63_, .-whatever_58_zero_63_


######### fixnum:zero? (mangled as "fixnum_58_zero_63_")
# io-no is 1
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl fixnum_58_zero_63_
  .type fixnum_58_zero_63_, @function
fixnum_58_zero_63_:
  #[...]
fixnum_58_zero_63__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 16(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*0(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $16, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size fixnum_58_zero_63_, .-fixnum_58_zero_63_


######### vector:blit-from-header (mangled as "vector_58_blit_45_from_45_header")
# io-no is 5
# leaf is #f
# local-no is 1
# scratch-no is 43
  .balign 8 #.align 2
  .globl vector_58_blit_45_from_45_header
  .type vector_58_blit_45_from_45_header, @function
vector_58_blit_45_from_45_header:
  #[...]
vector_58_blit_45_from_45_header_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 56(%rbx)
  # get-io: END
  # if-in: BEGIN
  movq 56(%rbx), %r11 # load the discriminand
  movq $0, %rax
  cmpq %r11, %rax
  je then73 # branch if equal
  # get-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 56(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 64(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $56, %rbx # pass frame pointer
  callq buffer_58_get
  addq $-56, %rbx # restore frame pointer
  # nontail-call: END
  # set-local: BEGIN
  movq 56(%rbx), %rax
  movq %rax, 48(%rbx)
  # set-local: END
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 56(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 64(%rbx)
  # get-io: END
  # get-local: BEGIN
  movq 48(%rbx), %rax
  movq %rax, 72(%rbx)
  # get-local: END
  # nontail-call: BEGIN
  addq $56, %rbx # pass frame pointer
  callq buffer_58_set_33_
  addq $-56, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 56(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 64(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $64, %rbx # pass frame pointer
  callq fixnum_58_1_43_
  addq $-64, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 72(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 80(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $80, %rbx # pass frame pointer
  callq fixnum_58_1_43_
  addq $-80, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 88(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $88, %rbx # pass frame pointer
  callq fixnum_58_1_45_
  addq $-88, %rbx # restore frame pointer
  # nontail-call: END
  # set-io: BEGIN
  movq 56(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 64(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 72(%rbx), %rax
  movq %rax, 16(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 80(%rbx), %rax
  movq %rax, 24(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 88(%rbx), %rax
  movq %rax, 32(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp vector_58_blit_45_from_45_header
  # tail-call: END
  jmp after74 # skip the "else" branch
then73:
  # return: BEGIN
  retq $0
  # return: END
after74:
  # if-in: END
################ END
  .size vector_58_blit_45_from_45_header, .-vector_58_blit_45_from_45_header


######### vector:append2 (mangled as "vector_58_append2")
# io-no is 2
# leaf is #f
# local-no is 3
# scratch-no is 43
  .balign 8 #.align 2
  .globl vector_58_append2
  .type vector_58_append2, @function
vector_58_append2:
  #[...]
vector_58_append2_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $48, %rbx # pass frame pointer
  callq vector_58_length
  addq $-48, %rbx # restore frame pointer
  # nontail-call: END
  # set-local: BEGIN
  movq 48(%rbx), %rax
  movq %rax, 24(%rbx)
  # set-local: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $48, %rbx # pass frame pointer
  callq vector_58_length
  addq $-48, %rbx # restore frame pointer
  # nontail-call: END
  # set-local: BEGIN
  movq 48(%rbx), %rax
  movq %rax, 32(%rbx)
  # set-local: END
  # get-local: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-local: END
  # get-local: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 56(%rbx)
  # get-local: END
  # nontail-call: BEGIN
  addq $48, %rbx # pass frame pointer
  callq fixnum_58__43_
  addq $-48, %rbx # restore frame pointer
  # nontail-call: END
  # nontail-call: BEGIN
  addq $48, %rbx # pass frame pointer
  callq vector_58_make
  addq $-48, %rbx # restore frame pointer
  # nontail-call: END
  # set-local: BEGIN
  movq 48(%rbx), %rax
  movq %rax, 40(%rbx)
  # set-local: END
  # get-local: BEGIN
  movq 40(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-local: END
  # get-value: BEGIN
  movq $1, %rax
  movq %rax, 56(%rbx)
  # get-value: END
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 64(%rbx)
  # get-io: END
  # get-value: BEGIN
  movq $1, %rax
  movq %rax, 72(%rbx)
  # get-value: END
  # get-local: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 80(%rbx)
  # get-local: END
  # nontail-call: BEGIN
  addq $48, %rbx # pass frame pointer
  callq vector_58_blit_45_from_45_header
  addq $-48, %rbx # restore frame pointer
  # nontail-call: END
  # get-local: BEGIN
  movq 40(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-local: END
  # get-local: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 56(%rbx)
  # get-local: END
  # nontail-call: BEGIN
  addq $56, %rbx # pass frame pointer
  callq fixnum_58_1_43_
  addq $-56, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 64(%rbx)
  # get-io: END
  # get-value: BEGIN
  movq $1, %rax
  movq %rax, 72(%rbx)
  # get-value: END
  # get-local: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 80(%rbx)
  # get-local: END
  # nontail-call: BEGIN
  addq $48, %rbx # pass frame pointer
  callq vector_58_blit_45_from_45_header
  addq $-48, %rbx # restore frame pointer
  # nontail-call: END
  # get-local: BEGIN
  movq 40(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-local: END
  # set-io: BEGIN
  movq 48(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size vector_58_append2, .-vector_58_append2


######### string:append2 (mangled as "string_58_append2")
# io-no is 2
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl string_58_append2
  .type string_58_append2, @function
string_58_append2:
  #[...]
string_58_append2_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp vector_58_append2
  # tail-call: END
################ END
  .size string_58_append2, .-string_58_append2


######### string:write (mangled as "string_58_write")
# io-no is 2
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl string_58_write
  .type string_58_write, @function
string_58_write:
  #[...]
string_58_write_TAIL:
################ BEGIN
  # nontail-call: BEGIN
  addq $24, %rbx # pass frame pointer
  callq io_58_standard_45_output
  addq $-24, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp io_58_write_45_string
  # tail-call: END
################ END
  .size string_58_write, .-string_58_write


######### io:standard-output (mangled as "io_58_standard_45_output")
# io-no is 1
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl io_58_standard_45_output
  .type io_58_standard_45_output, @function
io_58_standard_45_output:
  #[...]
io_58_standard_45_output_TAIL:
################ BEGIN
  # primitive: BEGIN
  movq 8*31(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $16, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size io_58_standard_45_output, .-io_58_standard_45_output


######### character:write (mangled as "character_58_write")
# io-no is 1
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl character_58_write
  .type character_58_write, @function
character_58_write:
  #[...]
character_58_write_TAIL:
################ BEGIN
  # nontail-call: BEGIN
  addq $16, %rbx # pass frame pointer
  callq io_58_standard_45_output
  addq $-16, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*37(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $16, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size character_58_write, .-character_58_write


######### e1:fail (mangled as "e1_58_fail")
# io-no is 0
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl e1_58_fail
  .type e1_58_fail, @function
e1_58_fail:
  #[...]
e1_58_fail_TAIL:
################ BEGIN
  # primitive: BEGIN
  movq 8*44(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $8, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size e1_58_fail, .-e1_58_fail


######### e1:error (mangled as "e1_58_error")
# io-no is 1
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl e1_58_error
  .type e1_58_error, @function
e1_58_error:
  #[...]
e1_58_error_TAIL:
################ BEGIN
  # get-value: BEGIN
  movq $p541, %rax
  movq %rax, 16(%rbx)
  # get-value: END
  # nontail-call: BEGIN
  addq $16, %rbx # pass frame pointer
  callq string_58_write
  addq $-16, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 16(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $16, %rbx # pass frame pointer
  callq string_58_write
  addq $-16, %rbx # restore frame pointer
  # nontail-call: END
  # get-global: BEGIN
  movq $p516, %rax # character:newline
  movq 2*8(%rax), %rax # Load global binding from symbol
  movq %rax, 16(%rbx)
  # get-global: END
  # nontail-call: BEGIN
  addq $16, %rbx # pass frame pointer
  callq character_58_write
  addq $-16, %rbx # restore frame pointer
  # nontail-call: END
  # tail-call: BEGIN
  jmp e1_58_fail
  # tail-call: END
################ END
  .size e1_58_error, .-e1_58_error


######### fixnum:< (mangled as "fixnum_58__60_")
# io-no is 2
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl fixnum_58__60_
  .type fixnum_58__60_, @function
fixnum_58__60_:
  #[...]
fixnum_58__60__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*10(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $24, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size fixnum_58__60_, .-fixnum_58__60_


######### fixnum:log10-helper (mangled as "fixnum_58_log10_45_helper")
# io-no is 2
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl fixnum_58_log10_45_helper
  .type fixnum_58_log10_45_helper, @function
fixnum_58_log10_45_helper:
  #[...]
fixnum_58_log10_45_helper_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-value: BEGIN
  movq $10, %rax
  movq %rax, 32(%rbx)
  # get-value: END
  # nontail-call: BEGIN
  addq $24, %rbx # pass frame pointer
  callq fixnum_58__60_
  addq $-24, %rbx # restore frame pointer
  # nontail-call: END
  # if-in: BEGIN
  movq 24(%rbx), %r11 # load the discriminand
  movq $0, %rax
  cmpq %r11, %rax
  je then75 # branch if equal
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
  jmp after76 # skip the "else" branch
then75:
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-value: BEGIN
  movq $10, %rax
  movq %rax, 32(%rbx)
  # get-value: END
  # nontail-call: BEGIN
  addq $24, %rbx # pass frame pointer
  callq fixnum_58__47_
  addq $-24, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $32, %rbx # pass frame pointer
  callq fixnum_58_1_43_
  addq $-32, %rbx # restore frame pointer
  # nontail-call: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp fixnum_58_log10_45_helper
  # tail-call: END
after76:
  # if-in: END
################ END
  .size fixnum_58_log10_45_helper, .-fixnum_58_log10_45_helper


######### fixnum:log10 (mangled as "fixnum_58_log10")
# io-no is 2
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl fixnum_58_log10
  .type fixnum_58_log10, @function
fixnum_58_log10:
  #[...]
fixnum_58_log10_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # if-in: BEGIN
  movq 24(%rbx), %r11 # load the discriminand
  movq $0, %rax
  cmpq %r11, %rax
  je then77 # branch if equal
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-value: BEGIN
  movq $0, %rax
  movq %rax, 32(%rbx)
  # get-value: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp fixnum_58_log10_45_helper
  # tail-call: END
  jmp after78 # skip the "else" branch
then77:
  # get-value: BEGIN
  movq $p497, %rax
  movq %rax, 24(%rbx)
  # get-value: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp e1_58_error
  # tail-call: END
after78:
  # if-in: END
################ END
  .size fixnum_58_log10, .-fixnum_58_log10


######### buffer:make (mangled as "buffer_58_make")
# io-no is 1
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl buffer_58_make
  .type buffer_58_make, @function
buffer_58_make:
  #[...]
buffer_58_make_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 16(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*4(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $16, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size buffer_58_make, .-buffer_58_make


######### vector:make (mangled as "vector_58_make")
# io-no is 1
# leaf is #f
# local-no is 1
# scratch-no is 43
  .balign 8 #.align 2
  .globl vector_58_make
  .type vector_58_make, @function
vector_58_make:
  #[...]
vector_58_make_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $24, %rbx # pass frame pointer
  callq fixnum_58_1_43_
  addq $-24, %rbx # restore frame pointer
  # nontail-call: END
  # nontail-call: BEGIN
  addq $24, %rbx # pass frame pointer
  callq buffer_58_make
  addq $-24, %rbx # restore frame pointer
  # nontail-call: END
  # set-local: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 16(%rbx)
  # set-local: END
  # get-local: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-local: END
  # get-value: BEGIN
  movq $0, %rax
  movq %rax, 32(%rbx)
  # get-value: END
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 40(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $24, %rbx # pass frame pointer
  callq buffer_58_set_33_
  addq $-24, %rbx # restore frame pointer
  # nontail-call: END
  # get-local: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-local: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size vector_58_make, .-vector_58_make


######### buffer:set! (mangled as "buffer_58_set_33_")
# io-no is 3
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl buffer_58_set_33_
  .type buffer_58_set_33_, @function
buffer_58_set_33_:
  #[...]
buffer_58_set_33__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 40(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*8(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $32, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size buffer_58_set_33_, .-buffer_58_set_33_


######### vector:set-from-header! (mangled as "vector_58_set_45_from_45_header_33_")
# io-no is 3
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl vector_58_set_45_from_45_header_33_
  .type vector_58_set_45_from_45_header_33_, @function
vector_58_set_45_from_45_header_33_:
  #[...]
vector_58_set_45_from_45_header_33__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 40(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 40(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 48(%rbx), %rax
  movq %rax, 16(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp buffer_58_set_33_
  # tail-call: END
################ END
  .size vector_58_set_45_from_45_header_33_, .-vector_58_set_45_from_45_header_33_


######### vector:set! (mangled as "vector_58_set_33_")
# io-no is 3
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl vector_58_set_33_
  .type vector_58_set_33_, @function
vector_58_set_33_:
  #[...]
vector_58_set_33__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 40(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $40, %rbx # pass frame pointer
  callq fixnum_58_1_43_
  addq $-40, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 40(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 48(%rbx), %rax
  movq %rax, 16(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp vector_58_set_45_from_45_header_33_
  # tail-call: END
################ END
  .size vector_58_set_33_, .-vector_58_set_33_


######### string:set! (mangled as "string_58_set_33_")
# io-no is 3
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl string_58_set_33_
  .type string_58_set_33_, @function
string_58_set_33_:
  #[...]
string_58_set_33__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 40(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 40(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 48(%rbx), %rax
  movq %rax, 16(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp vector_58_set_33_
  # tail-call: END
################ END
  .size string_58_set_33_, .-string_58_set_33_


######### character:fixnum->character (mangled as "character_58_fixnum_45__62_character")
# io-no is 2
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl character_58_fixnum_45__62_character
  .type character_58_fixnum_45__62_character, @function
character_58_fixnum_45__62_character:
  #[...]
character_58_fixnum_45__62_character_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-global: BEGIN
  movq $p361, %rax # character:0-as-character
  movq 2*8(%rax), %rax # Load global binding from symbol
  movq %rax, 32(%rbx)
  # get-global: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp fixnum_58__43_
  # tail-call: END
################ END
  .size character_58_fixnum_45__62_character, .-character_58_fixnum_45__62_character


######### fixnum:% (mangled as "fixnum_58__37_")
# io-no is 2
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl fixnum_58__37_
  .type fixnum_58__37_, @function
fixnum_58__37_:
  #[...]
fixnum_58__37__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*17(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $24, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size fixnum_58__37_, .-fixnum_58__37_


######### fixnum:/ (mangled as "fixnum_58__47_")
# io-no is 2
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl fixnum_58__47_
  .type fixnum_58__47_, @function
fixnum_58__47_:
  #[...]
fixnum_58__47__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*16(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $24, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size fixnum_58__47_, .-fixnum_58__47_


######### string:fill-with-positive-fixnum-digits! (mangled as "string_58_fill_45_with_45_positive_45_fixnum_45_digits_33_")
# io-no is 3
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl string_58_fill_45_with_45_positive_45_fixnum_45_digits_33_
  .type string_58_fill_45_with_45_positive_45_fixnum_45_digits_33_, @function
string_58_fill_45_with_45_positive_45_fixnum_45_digits_33_:
  #[...]
string_58_fill_45_with_45_positive_45_fixnum_45_digits_33__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # if-in: BEGIN
  movq 32(%rbx), %r11 # load the discriminand
  movq $0, %rax
  cmpq %r11, %rax
  je then79 # branch if equal
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 40(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $40, %rbx # pass frame pointer
  callq fixnum_58_1_45_
  addq $-40, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-io: END
  # get-value: BEGIN
  movq $10, %rax
  movq %rax, 56(%rbx)
  # get-value: END
  # nontail-call: BEGIN
  addq $48, %rbx # pass frame pointer
  callq fixnum_58__37_
  addq $-48, %rbx # restore frame pointer
  # nontail-call: END
  # nontail-call: BEGIN
  addq $48, %rbx # pass frame pointer
  callq character_58_fixnum_45__62_character
  addq $-48, %rbx # restore frame pointer
  # nontail-call: END
  # nontail-call: BEGIN
  addq $32, %rbx # pass frame pointer
  callq string_58_set_33_
  addq $-32, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 40(%rbx)
  # get-io: END
  # get-value: BEGIN
  movq $10, %rax
  movq %rax, 48(%rbx)
  # get-value: END
  # nontail-call: BEGIN
  addq $40, %rbx # pass frame pointer
  callq fixnum_58__47_
  addq $-40, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $48, %rbx # pass frame pointer
  callq fixnum_58_1_45_
  addq $-48, %rbx # restore frame pointer
  # nontail-call: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 40(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 48(%rbx), %rax
  movq %rax, 16(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp string_58_fill_45_with_45_positive_45_fixnum_45_digits_33_
  # tail-call: END
  jmp after80 # skip the "else" branch
then79:
  # return: BEGIN
  retq $0
  # return: END
after80:
  # if-in: END
################ END
  .size string_58_fill_45_with_45_positive_45_fixnum_45_digits_33_, .-string_58_fill_45_with_45_positive_45_fixnum_45_digits_33_


######### string:positive-fixnum->string (mangled as "string_58_positive_45_fixnum_45__62_string")
# io-no is 1
# leaf is #f
# local-no is 2
# scratch-no is 43
  .balign 8 #.align 2
  .globl string_58_positive_45_fixnum_45__62_string
  .type string_58_positive_45_fixnum_45__62_string, @function
string_58_positive_45_fixnum_45__62_string:
  #[...]
string_58_positive_45_fixnum_45__62_string_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # if-in: BEGIN
  movq 32(%rbx), %r11 # load the discriminand
  movq $0, %rax
  cmpq %r11, %rax
  je then81 # branch if equal
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $32, %rbx # pass frame pointer
  callq fixnum_58_log10
  addq $-32, %rbx # restore frame pointer
  # nontail-call: END
  # nontail-call: BEGIN
  addq $32, %rbx # pass frame pointer
  callq fixnum_58_1_43_
  addq $-32, %rbx # restore frame pointer
  # nontail-call: END
  jmp after82 # skip the "else" branch
then81:
  # get-value: BEGIN
  movq $1, %rax
  movq %rax, 32(%rbx)
  # get-value: END
after82:
  # if-in: END
  # set-local: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 16(%rbx)
  # set-local: END
  # get-local: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-local: END
  # nontail-call: BEGIN
  addq $32, %rbx # pass frame pointer
  callq vector_58_make
  addq $-32, %rbx # restore frame pointer
  # nontail-call: END
  # set-local: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 24(%rbx)
  # set-local: END
  # get-local: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-local: END
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 40(%rbx)
  # get-io: END
  # get-local: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-local: END
  # nontail-call: BEGIN
  addq $32, %rbx # pass frame pointer
  callq string_58_fill_45_with_45_positive_45_fixnum_45_digits_33_
  addq $-32, %rbx # restore frame pointer
  # nontail-call: END
  # get-local: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-local: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size string_58_positive_45_fixnum_45__62_string, .-string_58_positive_45_fixnum_45__62_string


######### fixnum:negate (mangled as "fixnum_58_negate")
# io-no is 1
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl fixnum_58_negate
  .type fixnum_58_negate, @function
fixnum_58_negate:
  #[...]
fixnum_58_negate_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 16(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*12(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $16, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size fixnum_58_negate, .-fixnum_58_negate


######### string:fixnum->string (mangled as "string_58_fixnum_45__62_string")
# io-no is 2
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl string_58_fixnum_45__62_string
  .type string_58_fixnum_45__62_string, @function
string_58_fixnum_45__62_string:
  #[...]
string_58_fixnum_45__62_string_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $24, %rbx # pass frame pointer
  callq fixnum_58_zero_63_
  addq $-24, %rbx # restore frame pointer
  # nontail-call: END
  # if-in: BEGIN
  movq 24(%rbx), %r11 # load the discriminand
  movq $0, %rax
  cmpq %r11, %rax
  je then83 # branch if equal
  # get-value: BEGIN
  movq $p253, %rax
  movq %rax, 24(%rbx)
  # get-value: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
  jmp after84 # skip the "else" branch
then83:
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-value: BEGIN
  movq $0, %rax
  movq %rax, 32(%rbx)
  # get-value: END
  # nontail-call: BEGIN
  addq $24, %rbx # pass frame pointer
  callq fixnum_58__60_
  addq $-24, %rbx # restore frame pointer
  # nontail-call: END
  # if-in: BEGIN
  movq 24(%rbx), %r11 # load the discriminand
  movq $0, %rax
  cmpq %r11, %rax
  je then85 # branch if equal
  # get-value: BEGIN
  movq $p542, %rax
  movq %rax, 24(%rbx)
  # get-value: END
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $32, %rbx # pass frame pointer
  callq fixnum_58_negate
  addq $-32, %rbx # restore frame pointer
  # nontail-call: END
  # nontail-call: BEGIN
  addq $32, %rbx # pass frame pointer
  callq string_58_positive_45_fixnum_45__62_string
  addq $-32, %rbx # restore frame pointer
  # nontail-call: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp string_58_append2
  # tail-call: END
  jmp after86 # skip the "else" branch
then85:
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp string_58_positive_45_fixnum_45__62_string
  # tail-call: END
after86:
  # if-in: END
after84:
  # if-in: END
################ END
  .size string_58_fixnum_45__62_string, .-string_58_fixnum_45__62_string


######### io:write-fixnum (mangled as "io_58_write_45_fixnum")
# io-no is 2
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl io_58_write_45_fixnum
  .type io_58_write_45_fixnum, @function
io_58_write_45_fixnum:
  #[...]
io_58_write_45_fixnum_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $32, %rbx # pass frame pointer
  callq string_58_fixnum_45__62_string
  addq $-32, %rbx # restore frame pointer
  # nontail-call: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp io_58_write_45_string
  # tail-call: END
################ END
  .size io_58_write_45_fixnum, .-io_58_write_45_fixnum


######### fixnum:+ (mangled as "fixnum_58__43_")
# io-no is 2
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl fixnum_58__43_
  .type fixnum_58__43_, @function
fixnum_58__43_:
  #[...]
fixnum_58__43__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*13(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $24, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size fixnum_58__43_, .-fixnum_58__43_


######### fixnum:- (mangled as "fixnum_58__45_")
# io-no is 2
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl fixnum_58__45_
  .type fixnum_58__45_, @function
fixnum_58__45_:
  #[...]
fixnum_58__45__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*14(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $24, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size fixnum_58__45_, .-fixnum_58__45_


######### fixnum:1- (mangled as "fixnum_58_1_45_")
# io-no is 1
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl fixnum_58_1_45_
  .type fixnum_58_1_45_, @function
fixnum_58_1_45_:
  #[...]
fixnum_58_1_45__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 16(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*2(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $16, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size fixnum_58_1_45_, .-fixnum_58_1_45_


######### fibo (mangled as "fibo")
# io-no is 2
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl fibo
  .type fibo, @function
fibo:
  #[...]
fibo_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # if-in: BEGIN
  movq 24(%rbx), %r11 # load the discriminand
  movq $0, %rax
  cmpq %r11, %rax
  je then87 # branch if equal
  movq $1, %rax
  cmpq %r11, %rax
  je then87 # branch if equal
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-value: BEGIN
  movq $2, %rax
  movq %rax, 32(%rbx)
  # get-value: END
  # nontail-call: BEGIN
  addq $24, %rbx # pass frame pointer
  callq fixnum_58__45_
  addq $-24, %rbx # restore frame pointer
  # nontail-call: END
  # nontail-call: BEGIN
  addq $24, %rbx # pass frame pointer
  callq fibo
  addq $-24, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $32, %rbx # pass frame pointer
  callq fixnum_58_1_45_
  addq $-32, %rbx # restore frame pointer
  # nontail-call: END
  # nontail-call: BEGIN
  addq $32, %rbx # pass frame pointer
  callq fibo
  addq $-32, %rbx # restore frame pointer
  # nontail-call: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp fixnum_58__43_
  # tail-call: END
  jmp after88 # skip the "else" branch
then87:
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
after88:
  # if-in: END
################ END
  .size fibo, .-fibo


######### whatever:eq? (mangled as "whatever_58_eq_63_")
# io-no is 2
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl whatever_58_eq_63_
  .type whatever_58_eq_63_, @function
whatever_58_eq_63_:
  #[...]
whatever_58_eq_63__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*1(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $24, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size whatever_58_eq_63_, .-whatever_58_eq_63_


######### fixnum:= (mangled as "fixnum_58__61_")
# io-no is 2
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl fixnum_58__61_
  .type fixnum_58__61_, @function
fixnum_58__61_:
  #[...]
fixnum_58__61__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*1(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $24, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size fixnum_58__61_, .-fixnum_58__61_


######### io:write-character (mangled as "io_58_write_45_character")
# io-no is 2
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl io_58_write_45_character
  .type io_58_write_45_character, @function
io_58_write_45_character:
  #[...]
io_58_write_45_character_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*37(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $24, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size io_58_write_45_character, .-io_58_write_45_character


######### vector:get-from-header (mangled as "vector_58_get_45_from_45_header")
# io-no is 2
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl vector_58_get_45_from_45_header
  .type vector_58_get_45_from_45_header, @function
vector_58_get_45_from_45_header:
  #[...]
vector_58_get_45_from_45_header_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp buffer_58_get
  # tail-call: END
################ END
  .size vector_58_get_45_from_45_header, .-vector_58_get_45_from_45_header


######### vector:get (mangled as "vector_58_get")
# io-no is 2
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl vector_58_get
  .type vector_58_get, @function
vector_58_get:
  #[...]
vector_58_get_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $32, %rbx # pass frame pointer
  callq fixnum_58_1_43_
  addq $-32, %rbx # restore frame pointer
  # nontail-call: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp vector_58_get_45_from_45_header
  # tail-call: END
################ END
  .size vector_58_get, .-vector_58_get


######### string:get (mangled as "string_58_get")
# io-no is 2
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl string_58_get
  .type string_58_get, @function
string_58_get:
  #[...]
string_58_get_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp vector_58_get
  # tail-call: END
################ END
  .size string_58_get, .-string_58_get


######### fixnum:1+ (mangled as "fixnum_58_1_43_")
# io-no is 1
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl fixnum_58_1_43_
  .type fixnum_58_1_43_, @function
fixnum_58_1_43_:
  #[...]
fixnum_58_1_43__TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 16(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*3(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $16, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size fixnum_58_1_43_, .-fixnum_58_1_43_


######### io:write-string-from (mangled as "io_58_write_45_string_45_from")
# io-no is 4
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl io_58_write_45_string_45_from
  .type io_58_write_45_string_45_from, @function
io_58_write_45_string_45_from:
  #[...]
io_58_write_45_string_45_from_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 40(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $40, %rbx # pass frame pointer
  callq fixnum_58__61_
  addq $-40, %rbx # restore frame pointer
  # nontail-call: END
  # if-in: BEGIN
  movq 40(%rbx), %r11 # load the discriminand
  movq $0, %rax
  cmpq %r11, %rax
  je then89 # branch if equal
  # return: BEGIN
  retq $0
  # return: END
  jmp after90 # skip the "else" branch
then89:
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 40(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 56(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $48, %rbx # pass frame pointer
  callq string_58_get
  addq $-48, %rbx # restore frame pointer
  # nontail-call: END
  # nontail-call: BEGIN
  addq $40, %rbx # pass frame pointer
  callq io_58_write_45_character
  addq $-40, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 40(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 56(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $56, %rbx # pass frame pointer
  callq fixnum_58_1_43_
  addq $-56, %rbx # restore frame pointer
  # nontail-call: END
  # get-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 64(%rbx)
  # get-io: END
  # set-io: BEGIN
  movq 40(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 48(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 56(%rbx), %rax
  movq %rax, 16(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 64(%rbx), %rax
  movq %rax, 24(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp io_58_write_45_string_45_from
  # tail-call: END
after90:
  # if-in: END
################ END
  .size io_58_write_45_string_45_from, .-io_58_write_45_string_45_from


######### buffer:get (mangled as "buffer_58_get")
# io-no is 2
# leaf is #t
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl buffer_58_get
  .type buffer_58_get, @function
buffer_58_get:
  #[...]
buffer_58_get_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 32(%rbx)
  # get-io: END
  # primitive: BEGIN
  movq 8*7(%r12), %rax # Load primitive address
  movq %rbx, %rdi
  addq $24, %rdi # pass the frame pointer
  callq *%rax
  # primitive: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size buffer_58_get, .-buffer_58_get


######### vector:length (mangled as "vector_58_length")
# io-no is 2
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl vector_58_length
  .type vector_58_length, @function
vector_58_length:
  #[...]
vector_58_length_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 24(%rbx)
  # get-io: END
  # get-value: BEGIN
  movq $0, %rax
  movq %rax, 32(%rbx)
  # get-value: END
  # set-io: BEGIN
  movq 24(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 32(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp buffer_58_get
  # tail-call: END
################ END
  .size vector_58_length, .-vector_58_length


######### string:length (mangled as "string_58_length")
# io-no is 1
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl string_58_length
  .type string_58_length, @function
string_58_length:
  #[...]
string_58_length_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 16(%rbx)
  # get-io: END
  # set-io: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp vector_58_length
  # tail-call: END
################ END
  .size string_58_length, .-string_58_length


######### io:write-string (mangled as "io_58_write_45_string")
# io-no is 4
# leaf is #f
# local-no is 0
# scratch-no is 43
  .balign 8 #.align 2
  .globl io_58_write_45_string
  .type io_58_write_45_string, @function
io_58_write_45_string:
  #[...]
io_58_write_45_string_TAIL:
################ BEGIN
  # get-io: BEGIN
  movq 0(%rbx), %rax
  movq %rax, 40(%rbx)
  # get-io: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 48(%rbx)
  # get-io: END
  # get-value: BEGIN
  movq $0, %rax
  movq %rax, 56(%rbx)
  # get-value: END
  # get-io: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 64(%rbx)
  # get-io: END
  # nontail-call: BEGIN
  addq $64, %rbx # pass frame pointer
  callq string_58_length
  addq $-64, %rbx # restore frame pointer
  # nontail-call: END
  # set-io: BEGIN
  movq 40(%rbx), %rax
  movq %rax, 0(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 48(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 56(%rbx), %rax
  movq %rax, 16(%rbx)
  # set-io: END
  # set-io: BEGIN
  movq 64(%rbx), %rax
  movq %rax, 24(%rbx)
  # set-io: END
  # tail-call: BEGIN
  jmp io_58_write_45_string_45_from
  # tail-call: END
################ END
  .size io_58_write_45_string, .-io_58_write_45_string


######### f (mangled as "f")
# io-no is 0
# leaf is #f
# local-no is 1
# scratch-no is 43
  .balign 8 #.align 2
  .globl f
  .type f, @function
f:
  #[...]
f_TAIL:
################ BEGIN
  # nontail-call: BEGIN
  addq $16, %rbx # pass frame pointer
  callq io_58_standard_45_output
  addq $-16, %rbx # restore frame pointer
  # nontail-call: END
  # set-local: BEGIN
  movq 16(%rbx), %rax
  movq %rax, 8(%rbx)
  # set-local: END
  # get-local: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 16(%rbx)
  # get-local: END
  # get-value: BEGIN
  movq $34, %rax
  movq %rax, 24(%rbx)
  # get-value: END
  # nontail-call: BEGIN
  addq $24, %rbx # pass frame pointer
  callq fibo
  addq $-24, %rbx # restore frame pointer
  # nontail-call: END
  # nontail-call: BEGIN
  addq $16, %rbx # pass frame pointer
  callq io_58_write_45_fixnum
  addq $-16, %rbx # restore frame pointer
  # nontail-call: END
  # get-local: BEGIN
  movq 8(%rbx), %rax
  movq %rax, 16(%rbx)
  # get-local: END
  # get-value: BEGIN
  movq $p17, %rax
  movq %rax, 24(%rbx)
  # get-value: END
  # nontail-call: BEGIN
  addq $16, %rbx # pass frame pointer
  callq io_58_write_45_string
  addq $-16, %rbx # restore frame pointer
  # nontail-call: END
  # return: BEGIN
  retq $0
  # return: END
################ END
  .size f, .-f

