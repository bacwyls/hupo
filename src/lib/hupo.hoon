/-  *hupo
|%
++  hupo
!:
|_  stat=pkstat
++  init-game
  :: players, entropy, starting chip amount, smallblind amount
  |=  [plrs=(list @p) eny=@uv cpamt=@ sbamt=@]
  ^-  pkstat
  =.  delr.stat   (init-delr eny)
  =.  plrs.stat   (init-plrs plrs cpamt)
  =.  meta.stat   (init-meta sbamt eny)
  =.  stat        deal-pkts
  =.  stat        bet-blinds
  stat
++  init-meta
  |=  [sbamt=@ eny=@uv]
  ^-  pkmeta
  =/  random  ~(. og eny)
  =^  sbwho  random  (rads:random 2)  :: assumes 2 players
  :: 3 = (sb) + (bb) + player choice
  [sbwho sbwho 3 sbamt %preflop ~]
++  reset-meta
  ^-  pkmeta
  =*  sbwho  sbwho.meta.stat
  =*  sbamt  sbamt.meta.stat
  =.  sbwho  (next-pidx sbwho)
  [sbwho sbwho 3 sbamt %preflop ~]
++  init-plrs
  |=  [plrs=(list @p) cpamt=@]
  ^-  (list pkplyr)
  |-
  ?~  plrs  ~
  =|  =pkplyr
  =.  who.pkplyr  i.plrs
  =.  has.pkplyr  cpamt
  =.  in.pkplyr  &
  =.  move.pkplyr  ~
  :-  pkplyr
    $(plrs t.plrs)
++  init-delr
  |=  eny=@
  ^-  pkdelr
  [main=(shuffle-deck make-deck eny) bord=~]
++  get-plyr-stat
  |=  who=@p  :: set all decks to null besides who's pocket and bord
  ^-  pkstat
  =.  main.delr.stat  ~
  =.  plrs.stat
    |-  :: TODO this should be a roll
    ?~  plrs.stat  ~
    =?  pokt.i.plrs.stat
      ?!(=(who.i.plrs.stat who))  ~
    [i.plrs.stat $(plrs.stat t.plrs.stat)]
  stat

++  bet-call
  |=  who=@p
  ^-  pkstat
  =.  stat  (set-plyr-move who %call)
  =+  max=get-max-bet
  =/  plyr=pkplyr  (get-plyr who)
  =+  diff=(sub max bet.plyr)
  (plyr-bet-meta plyr diff)
++  bet-check
  |=  who=@p
  ^-  pkstat
  =.  stat  (set-plyr-move who %check)
  =/  plyr=pkplyr  (get-plyr who)
  (plyr-bet-meta plyr 0)
++  can-plyr-check
  |=  who=@p
  ^-  ?
  =/  wp  (get-plyr who)
  ?|  =(bet.wp get-max-bet) 
      =(has.wp 0)
    ==
++  bet-raise
  |=  [who=@p amt=@]
  ^-  pkstat
  =.  stat  (set-plyr-move who %raise)
  =+  max=get-max-bet
  =/  plyr=pkplyr  (get-plyr who)
  =+  diff=(sub max bet.plyr)
  (plyr-bet-meta plyr (add diff amt))
::
:: if only one other bet can happen, this fold makes them the winner
++  bet-fold
  |=  who=@p
  ^-  pkstat
  =.  stat  (set-plyr-move who %fold)
  =/  plyr=pkplyr  (get-plyr who)
  =.  stat  (plyr-bet-meta plyr 0)
  =?  btctr.meta.stat  =(btctr.meta.stat 1)  0 
  =.  in.plyr  %.n
  =.  plrs.stat  (set-plyr plyr)
  stat
::
:: update move for a given player
:: set move to ~ for others
++  set-plyr-move
  |=  [who=@p =pkmove]
  ^-  pkstat
  =/  mp  (get-plyr who)
  =.  move.mp  pkmove
  =.  plrs.stat  (set-plyr mp)
  =.  plrs.stat
    |-  ?~  plrs.stat  ~
      =?  move.i.plrs.stat
        ?!(=(who.i.plrs.stat who))
          ~
      :-  i.plrs.stat
      $(plrs.stat t.plrs.stat)
  stat 
++  get-max-bet
  ^-  @
  =*  plrs  plrs.stat
  =/  max=@  0
  |-
  ?~  plrs  max
  =?  max  (gte bet.i.plrs max)
    bet.i.plrs
  $(plrs t.plrs)
++  get-better
  ^-  @p  :: returns player that is currently betting
  =*  btwho  btwho.meta.stat
  ?:  (gth btwho (lent plrs.stat))  !!
  =+  p=(snag btwho plrs.stat)
  who.p
::
:: iterates plrs.stat until finds a matching @p, otherwise crash
++  get-plyr
  |=  who=@p
  ^-  pkplyr  
  =*  plrs  plrs.stat
  |-
  ?~  plrs  !!
  ?:  =(who.i.plrs who)  i.plrs
    $(plrs t.plrs)
::
:: iterate until matching player is found, return index of that player in list
++  get-plyr-idx
  |=  myplyr=pkplyr
  ^-  @
  =*  plrs  plrs.stat
  =+  idx=0
  |-
  ?~  plrs  !!
  ?:  =(who.i.plrs who.myplyr)  idx
    $(plrs t.plrs, idx +(idx))
++  end-deal-round
  |=  eny=@uv
  ^-  pkstat
  ::  deal any remaining darcs to bord
  ::  set wnrs.meta.stat to get-winner
  =.  rwnd.meta.stat  %showdown
  =.  delr.stat  (deal-full-bord eny)
  =.  stat  get-winners
  stat
::
:: payout winners
:: reset darcs and bet amts
++  reset-deal-round
  |=  eny=@uv
  ^-  pkstat
  :: 
  =.  plrs.stat
  |-
    ?~  plrs.stat  ~
    =.  bet.i.plrs.stat  0
    =.  pokt.i.plrs.stat  ~
    =.  move.i.plrs.stat  ~
    =.  hval.i.plrs.stat  *hval
    =.  in.i.plrs.stat    &
    :-  i.plrs.stat
    $(plrs.stat t.plrs.stat)
  :: 
  =*  wnrs  wnrs.meta.stat
  =.  stat
  |-
    ?~  wnrs  stat
    =.  plrs.stat
    |-
      ?~  plrs.stat  ~
      =?  has.i.plrs.stat  =(who.i.plrs.stat who.i.wnrs)
          (add has.i.plrs.stat amt.i.wnrs)
      :-  i.plrs.stat
      $(plrs.stat t.plrs.stat)
    $(wnrs t.wnrs)
  ::
  =.  delr.stat   (init-delr eny)
  =.  meta.stat   reset-meta
  =.  stat        deal-pkts
  =.  stat        bet-blinds
  stat    
::
:: move to next rwnd, deal appropriate cards, reset btwho to sbwho, 
++  end-bet-round
  |=  eny=@uv
  ^-  pkstat
  =.  rwnd.meta.stat  get-next-pkrwnd
  =+  bbwho=(next-pidx sbwho.meta.stat)
  =.  btwho.meta.stat  bbwho
  =.  btctr.meta.stat  how-many-in
  :: if we're entering %showdown, btctr is 0
  =?  btctr.meta.stat  =(rwnd.meta.stat %showdown)  0
  =?  delr.stat     ?!(=(rwnd.meta.stat %showdown))
    (deal-to-board eny)
  stat
::
++  next-btwho
  :: get next-pidx until the plyr has in=%.y
  ^-  @
  =+  next=(next-pidx btwho.meta.stat)
  |-
  =+  plyr=(snag next plrs.stat)
  ?:  in.plyr  next
    $(next (next-pidx next))
::
++  dec-btctr
  ^-  @
  ?:  =(btctr.meta.stat 0)  0
  (sub btctr.meta.stat 1)
::
:: TODO when dealing to bord, main should be shuffled first with fresh eny.
:: game host shouldn't know result
++  deal-to-board
  |=  eny=@uv
  ^-  pkdelr
  =*  rwnd  rwnd.meta.stat
  =/  dealt=deck  ~
  =+  amt-to-deal=1
  =?  amt-to-deal  =(rwnd %flop)  3
  =.  main.delr.stat  (shuffle-deck main.delr.stat eny)
  =^  dealt  main.delr.stat  (draw amt-to-deal main.delr.stat)
  =.  bord.delr.stat  (weld bord.delr.stat hand.dealt)
  delr.stat
++  deal-full-bord
  |=  eny=@uv
  ^-  pkdelr
  |-
  ?:  (lth (lent bord.delr.stat) 5)
    $(delr.stat (deal-to-board eny))
  delr.stat
++  get-next-pkrwnd
  ^-  pkrwnd
  =*  rwnd  rwnd.meta.stat
  ?:  =(rwnd %preflop)      %flop
  ?:  =(rwnd %flop)         %turn
  ?:  =(rwnd %turn)         %river
  ?:  =(rwnd %river)    %showdown
  !!
::
:: all "in" players must have equal "bet" or have 0 "has" (all-in).
:: ignore not-in players
:: bet counter must be 0 to move on. aka every "in" player must have had a chance to bet
++  is-bet-round-over
  ^-  ?  :: is the betting round over?
  ?:  (gth btctr.meta.stat 0)  %.n
  ?:  %+  lien
        plrs.stat 
        |=  =pkplyr
        =(move.pkplyr %check)
    %.y
  =+  max=get-max-bet
  =*  plrs  plrs.stat
  |-
  ?~  plrs  %.y
  ?:  ?!(in.i.plrs)      $(plrs t.plrs)
  ?:  =(max bet.i.plrs)  $(plrs t.plrs)
  ?:  =(0 has.i.plrs)   $(plrs t.plrs)  :: all in
  %.n
::
:: is it time to determine round winner and payout
++  is-deal-round-over
  ^-  ?
  :: i.e. theres only one "in" player
  :: or the current rwnd is %showdown
  :: or everyone is all in
  :: TODO, account for player with less chips going all in
  :: atm they have to check repeatedly until the round ends
  ?:  (lte how-many-in 1)  %.y
  ?:  =(rwnd.meta.stat %showdown)  %.y
  ?:  =(how-many-all-in how-many-in)  %.y
  %.n
::
:: two player logic
:: if showdown and losing player has 0 chips
++  is-game-over
  ^-  ?
  ?.  =(rwnd.meta.stat %showdown)
    |
  ::
  ?.  (gth (lent wnrs.meta.stat) 0)
    |
  =/  wnr  (snag 0 wnrs.meta.stat)
  |-  ?~  plrs.stat  |
  ?.  =(who.i.plrs.stat who.wnr)
    =(has.i.plrs.stat 0)
  $(plrs.stat t.plrs.stat)
::
::  number of plrs with in=%.y
++  how-many-in
  ^-  @
  =*  plrs  plrs.stat
  =+  amt=0
  |-
  ?~  plrs  amt
  ?:  in.i.plrs  $(plrs t.plrs, amt +(amt))
    $(plrs t.plrs)
++  how-many-all-in
  ^-  @
  ::  number of "in" plyrs with has=0
  =*  plrs  plrs.stat
  =+  amt=0
  |-
  ?~  plrs  amt
  ?:  ?&(in.i.plrs =(has.i.plrs 0))  $(plrs t.plrs, amt +(amt))
    $(plrs t.plrs)
++  is-playing
  |=  who=@p
  ^-  ?
  =*  plrs  plrs.stat
  |-
  ?~  plrs  !!
  ?:  =(who.i.plrs who)  in.i.plrs
    $(plrs t.plrs)
++  bet-blinds
  ^-  pkstat
  =*  sbamt  sbamt.meta.stat
  =*  sbwho  sbwho.meta.stat
  =*  plrs   plrs.stat
  =+  bbwho=(next-pidx sbwho)
  =.  stat  (plyr-bet-meta (snag sbwho plrs) sbamt)
  =.  stat  (plyr-bet-meta (snag bbwho plrs) (mul 2 sbamt))
  stat
++  deal-pkts  :: deal pockets
  ^-  pkstat
  =+  idx=0
  |-
  ?:  =(idx (lent plrs.stat))  stat
  $(stat (deal-to idx), idx +(idx))
++  deal-to
  |=  pidx=@  :: player index in plrs
  ^-  pkstat
  =/  plyr=pkplyr  (snag pidx plrs.stat)
  =^  pokt  main.delr.stat  (draw 2 main.delr.stat)
  =.  pokt.plyr  pokt
  =.  plrs.stat  (set-plyr plyr)
  stat

::
:: increment player index, looping back to 0 when exceeding length
++  next-pidx
  |=  pidx=@
  ^-  @
  =+  next=+(pidx)
  =?  next  (gth next (sub (lent plrs.stat) 1))  0
  next
++  set-plyr
  |=  [plyr=pkplyr]
  ^-  (list pkplyr)
  =*  plrs  plrs.stat
  |-
  ?~  plrs  ~
  ?:  =(who.i.plrs who.plyr)
    [i=plyr t=$(plrs t.plrs)]
  [i=i.plrs t=$(plrs t.plrs)]
++  plyr-bet-trivial
  |=  [plyr=pkplyr amt=@]
  ^-  pkplyr
  ?:  ?!(in.plyr)  !!  :: crash if player isnt in-play
  ?:  (gth amt has.plyr)  !!  :: crash if player doesn't have enough
  =.  has.plyr  (sub has.plyr amt)
  =.  bet.plyr  (add bet.plyr amt)
  plyr
++  plyr-bet-meta
  |=  [plyr=pkplyr amt=@]
  ^-  pkstat
  =?  amt  (gth amt has.plyr)  has.plyr   :: all in
  =.  plyr  (plyr-bet-trivial plyr amt)
  =.  plrs.stat  (set-plyr plyr)
  =.  btctr.meta.stat  dec-btctr
  =.  btwho.meta.stat  next-btwho
  stat
++  get-pot
  ^-  @
  :: accumulate "bet" from every player in plrs.stat
  =*  plrs  plrs.stat
  =+  pot=0
  |-
  ?~  plrs  pot
  $(plrs t.plrs, pot (add pot bet.i.plrs))
::
:: if theres one player, they win the whole pot
:: otherwise, get hand value for each "in" player
++  get-winners
  ^-  pkstat
  ?:  =(how-many-in 1)
    =.  wnrs.meta.stat
      :_  ~
      |-
      ?~  plrs.stat  !!
      ?:  in.i.plrs.stat
        [who.i.plrs.stat get-pot]
      $(plrs.stat t.plrs.stat)
    stat
  =.  plrs.stat
    |-
    ?~  plrs.stat  ~
    =?  hval.i.plrs.stat  in.i.plrs.stat
      (plyr-hval i.plrs.stat)
    [i.plrs.stat $(plrs.stat t.plrs.stat)]
  ::
  =/  who-best-hand=(unit @p)  compare-hvals
  ?~  who-best-hand
    =.  wnrs.meta.stat  pot-split
    stat
  =.  wnrs.meta.stat  ~[[u.who-best-hand get-pot]]
  stat
::
:: :: compare-hvals
:: rank.hval or a.hval or b.hval or *kicker
::   *kicker checks for n kick cards
::      e.g. a pair can have 3, a set 2
:: 
:: hval meanings
:: -------------
:: rank | name    | a      | b       |
:: --------------------------
:: 0    | high    | ~      | ~       |
:: 1    | pair    | pair   | ~       |
:: 2    | twopair | hpair  | lpair   |  *higher pair, lower pair
:: 3    | set     | set    | ~       |
:: 4    | str8    | ~      | ~       |
:: 5    | flush   | ~      | ~       |
:: 6    | fullh   | set    | pair    |
:: 7    | quads   | quads  | ~       |
:: 8    | sflush  | ~      | ~       |
:: 9    | rflush  | ~      | ~       |
::
++  compare-hvals
  ^-  (unit @p) 
  :: get @ max-hval-rank
  =/  max-hval-rank=@  0
  =.  max-hval-rank
  |-
    ?~  plrs.stat  max-hval-rank
    ?:  (gth rank.hval.i.plrs.stat max-hval-rank)
      $(plrs.stat t.plrs.stat, max-hval-rank rank.hval.i.plrs.stat)
    $(plrs.stat t.plrs.stat)
  =/  wnrs=(list @p)
  |-
    ?~  plrs.stat  ~
    ?:  =(rank.hval.i.plrs.stat max-hval-rank)
      :-
      who.i.plrs.stat
      $(plrs.stat t.plrs.stat)
    $(plrs.stat t.plrs.stat)
  ::
  ?.  (gth (lent wnrs) 1)
    [~ (snag 0 wnrs)]
  =|  wnr=(unit @p) 
  ::
  :: switch on max-hval rank
  ?:  =(0 max-hval-rank)
    (do-kicker ~)
  ?:  =(1 max-hval-rank)
    =.  wnr  get-hi-a
    ?~  wnr  (do-kicker [~ 3])    :: 3 kicker spots in a pair
    wnr
  ?:  =(2 max-hval-rank)
    =.  wnr  get-hi-a
    ?~  wnr
      =/  wnrb=(unit @p)  get-hi-b
      ?~  wnrb
      (do-kicker [~ 1])
      wnrb
    wnr
  ?:  =(3 max-hval-rank)
    =.  wnr  get-hi-a
    ?~  wnr  (do-kicker [~ 2])    :: 2 kicker spots in a set
    wnr
  ?:  =(4 max-hval-rank)
    (do-kicker ~)                 :: high card is kicker in a straight
  ?:  =(5 max-hval-rank)
    (do-kicker ~)                 :: high card is kicker in a flush
  ?:  =(6 max-hval-rank)
    =.  wnr  get-hi-a
    ?~  wnr
      =/  wnrb=(unit @p)  get-hi-b
      ?~  wnrb
        ~                         :: no room for a kicker, just tie
      wnrb
    wnr
  ?:  =(7 max-hval-rank)
    =.  wnr  get-hi-a
    ?~  wnr  (do-kicker [~ 1])    :: 1 kicker spot in four of a kind
    wnr
  ::
  :: straight flush / royal flush
  :: do-kicker will either find a tie or find the @p with high card
  (do-kicker ~)
::
++  get-hi-a
  ^-  (unit @p)
  =/  pa=pkplyr  (snag 0 plrs.stat)
  =/  pb=pkplyr  (snag 1 plrs.stat)
  ?~  a.hval.pa  !!
  ?~  a.hval.pb  !!
  ?:  =(val.i.a.hval.pa val.i.a.hval.pb)
    ~
  ?:  (gth val.i.a.hval.pa val.i.a.hval.pb)
    [~ who.pa]
  [~ who.pb]
::
++  get-hi-b
  ^-  (unit @p)
  =/  pa=pkplyr  (snag 0 plrs.stat)
  =/  pb=pkplyr  (snag 1 plrs.stat)
  ?~  b.hval.pa  !!
  ?~  b.hval.pb  !!
  ?:  =(val.i.b.hval.pa val.i.b.hval.pb)
    ~
  ?:  (gth val.i.b.hval.pa val.i.b.hval.pb)
    [~ who.pa]
  [~ who.pb]
::
:: :: do-kicker
:: create pile
:: remove a and b of each player hval from pile
:: i=14
:: while(i>1)
::   (who-has-val i pile)
::   i--
++  do-kicker
  |=  kick-limit=(unit @ud)
  ^-  (unit @p)
  =/  pile=deck  make-darc-pile
  =.  pile  (deck-rmv-a-b pile)
  =.  pile  (deck-sort pile)
  =+  val=14
  =/  kicks=@ud
    ?~  kick-limit  5
      u.kick-limit
  |-
    ?:  ?|  =(0 kicks)
            =(0 val)
        ==
      :: out of kicks
      ~
    ?.  (deck-has-val val pile)
      $(val (dec val)) 
    =/  whos=(list @p)
        (who-has-val val pile)
    ?:  =(1 (lent whos))
      :: TODO could add a kicker to gsta here
      [~ (snag 0 whos)]  :: WINNER
    ::
    :: kicker was either delr owned or tied
    $(val (dec val), kicks (dec kicks))
::
++  deck-rmv-a-b
  |=  =deck
  ^-  ^deck
  :: this only works for two players
  =/  pa=pkplyr  (snag 0 plrs.stat)
  =/  pb=pkplyr  (snag 1 plrs.stat)
  =.  deck
  |-  ?~  deck  ~
  ?.  ?&
      =(~ (find [i.deck]~ a.hval.pa))
      =(~ (find [i.deck]~ b.hval.pa))
      =(~ (find [i.deck]~ a.hval.pb))
      =(~ (find [i.deck]~ b.hval.pb))
      ==
    $(deck t.deck)
  :: i.deck is not in any a.hval or b.hval
  :-  i.deck
    $(deck t.deck)
  deck
::
:: who owns darcs of val in deck
:: return ~ means the delr owns it
:: find matching darcs
:: return owners of matching darcs
::
++  who-has-val
  |=  [val=@ud pile=deck]
  ^-  (list @p)
  =/  valpile=deck
      (get-darcs-with-val val pile)
  =|  whohas=(list @p)
  |-
    ?~  valpile  whohas
    =/  ownr=(unit @p)
      (who-owns-darc i.valpile)
    ?~  ownr
      :: delr owned, continue
      $(valpile t.valpile)
    :: stick plyr in list if plyr isnt already in list
    ?~  (find [u.ownr]~ whohas)
      $(valpile t.valpile, whohas (weld whohas [u.ownr]~))
    $(valpile t.valpile)
::
:: if there is no plyr owner
:: return ~
::
++  who-owns-darc  
  |=  =darc
  ^-  (unit @p)
  |-  ?~  plrs.stat  ~
  ?:  ?|  =(darc (snag 0 pokt.i.plrs.stat))
          =(darc (snag 1 pokt.i.plrs.stat))
      ==
    [~ who.i.plrs.stat]
  $(plrs.stat t.plrs.stat)
::
++  get-darcs-with-val
  |=  [val=@ud pile=deck]
  ^-  deck
  |-  ?~  pile  ~
  ?:  =(val val.i.pile)
    :-  i.pile
    $(pile t.pile)
  $(pile t.pile)
::
++  make-darc-pile
  ^-  deck
  :: this is implemented for only two players
  =/  pa=pkplyr  (snag 0 plrs.stat)
  =/  pb=pkplyr  (snag 1 plrs.stat)
  %+  weld  bord.delr.stat
  (weld pokt.pa pokt.pb)
::  
++  deck-has-val
  |=  [val=@ud =deck]
  ^-  ?
  =?  val  =(val 1)  14  :: treat 1 as 14
  |-  ?~  deck  %.n
  ?:  =(val.i.deck val)  %.y
    $(deck t.deck)
++  pot-split
  ^-  (list pkwinr)
  =+  pot=get-pot
  =+  payout=(div pot (lent plrs.stat))
  |-
  ?~  plrs.stat  ~
  :-
  :-  who.i.plrs.stat  payout
  $(plrs.stat t.plrs.stat)
::
++  plyr-hval
  |=  plyr=pkplyr
  =/  hand=deck  (full-poker-hand plyr)
  (get-hand-value hand)
++  full-poker-hand
  |=  plyr=pkplyr
  (weld pokt.plyr bord.delr.stat)
::
::
:: begin playing-cards
:: base from https://urbit.org/docs/hoon/hoon-school/libraries/
++  make-deck
  ^-  deck
  =|  mydeck=deck
  =/  i  1
  |-
  ?:  (gth i 4)
    mydeck
  =/  j  2
  |-
  ?.  (lte j 14)
    ^$(i +(i))
  %=  $
    j       +(j)
    mydeck  [(num-to-suit i) j]^mydeck
  ==
++  num-to-suit
  |=  val=@ud
  ^-  suit
  ?+  val  !!
    %1  %hart
    %2  %spad
    %3  %club
    %4  %diam
  ==
++  suit-to-num
  |=  sut=suit
  ^-  @ud
  ?:  =(sut %hart)  1
  ?:  =(sut %spad)  2
  ?:  =(sut %club)  3
  4
++  shuffle-deck
  |=  [unshuffled=deck entropy=@]
  ^-  deck
  =|  shuffled=deck
  =/  random  ~(. og entropy)
  =/  remaining  (lent unshuffled)
  |-
  ?:  =(remaining 1)
    :_  shuffled
    (snag 0 unshuffled)
  =^  index  random  (rads:random remaining)
  %=  $
    shuffled      (snag index unshuffled)^shuffled
    remaining     (dec remaining)
    unshuffled    (oust [index 1] unshuffled)
  ==
++  draw
  |=  [n=@ud d=deck]
  ^-  [hand=deck rest=deck]
  :-  (scag n d)
  (slag n d)
++  deck-sort
  |=  mydeck=deck
  ^-  deck
  %+  sort  mydeck
    |=  [a=darc b=darc]
    ^-  ?
    (gte val.a val.b)
++  suit-to-tape
  |=  mysuit=suit
  ?:  =(mysuit %hart)  "<"
  ?:  =(mysuit %spad)  "~"
  ?:  =(mysuit %club)  "%"
                       "="
++  rank-to-tape
  |=  rank=@ud
  ?+  rank  !!
    %1   "A"
    %2   "2"
    %3   "3"
    %4   "4"
    %5   "5"
    %6   "6"
    %7   "7"
    %8   "8"
    %9   "9"
    %10  "T"
    %11  "J"
    %12  "Q"
    %13  "K"
    %14  "A"
  ==
::
:: " "+val.darc+"."+sut.darc+" "
++  darc-to-cord
  |=  =darc
  ^-  cord
  %:  crip
    %+  weld
      %+  weld
        " "
        %+  weld
          (rank-to-tape val.darc)
          %+  weld
            "."
            (suit-to-tape sut.darc)
      " "
  ==
++  deck-to-tape
  |=  [=deck size=@ud]
  ^-  (list cord)
  =/  truesize  (lent deck) 
  =+  iter=0
  |-
  ?:  =(iter size)  ~
  ?:  (gte iter truesize)
    :-  ' _._ '
    $(iter +(iter))
  :-  (darc-to-cord (snag iter deck))
    $(iter +(iter))
::
++  hval-to-tape
  |=  =hval
  ?+  rank.hval  !!
    %0  "High Card"
    %1  "Pair"
    %2  "Two Pair"
    %3  "Three of a Kind"
    %4  "Straight"
    %5  "Flush"
    %6  "Full House"
    %7  "Four of a Kind"
    %8  "Straight Flush"
    %9  "Royal Flush"
  ==
++  get-hand-value
  |=  mydeck=deck
  ^-  hval
  |^
  =.  mydeck  (deck-sort mydeck)
  =|  res=(unit hval)
  ::
  ::  checking from most valuable to least valuable
  ::  if returns ~, it is not of that value
  ::  so continue to next check
  ::
  =+  res=(is-royal-flush mydeck)
  ?~  res
    =+  res=(is-straight-flush mydeck)
    ?~  res
      =+  res=(is-four-kind mydeck)
      ?~  res
        =+  res=(is-full-house mydeck)
        ?~  res
          =+  res=(is-flush mydeck)
          ?~  res
            =+  res=(is-straight mydeck)
            ?~  res
              =+  res=(is-set mydeck)
              ?~  res
                =+  res=(is-two-pair mydeck)
                ?~  res
                  =+  res=(is-pair mydeck)
                  ?~  res
                    [0 ~ ~]  :: hval for highcard
                  u.res
                u.res
              u.res
            u.res
          u.res
        u.res
      u.res
    u.res
  u.res
::
::
:: begin hval rank checking arms.
:: each arm takes a sorted deck
:: and returns an hval
:: if returned hval is bunt
:: it means the input deck
:: is not of the checked rank
::
  ++  is-high-card
    |=  mydeck=deck
    ^-  (unit hval)
    [~ [0 ~ ~]]
  ++  is-pair
    |=  mydeck=deck
    ^-  (unit hval)
    =+  rank=1
    ?:  =(0 (lent mydeck))
        ~
    =/  prev=darc  (snag 0 mydeck)
    =.  mydeck  (slag 1 mydeck)
    |-
    ?:  =((lent mydeck) 0)
      ~
    =+  next=(snag 0 mydeck)
    ?:  =(val.prev val.next)
      [~ [rank [prev next ~] ~]]
    $(prev next, mydeck (slag 1 mydeck))
  ++  is-two-pair
    |=  mydeck=deck
    ^-  (unit hval)
    =+  rank=2
    ::  find a pair, then call is-pair on remainder of deck
    =/  prev=darc  (snag 0 mydeck)
    =.  mydeck  (slag 1 mydeck)
    |-
    ?:  (lte (lent mydeck) 3)
      ~
    =+  next=(snag 0 mydeck)
    ?:  =(val.prev val.next)
      =/  res=(unit hval)  (is-pair (slag 1 mydeck))
      ?~  res  ~
      ?:  =(rank.u.res 1)
        [~ [rank [prev next ~] a.u.res]]
      ~
    $(prev next, mydeck (slag 1 mydeck))
  ++  is-set
    |=  mydeck=deck
    ^-  (unit hval)
    =+  rank=3
    ::  since deck is sorted
    ::  check for 3 consecutive cards
    =/  prev=darc  (snag 0 mydeck)
    =.  mydeck  (slag 1 mydeck)
    |-
    ?:  (lth (lent mydeck) 2)
      ~
    =+  first=(snag 0 mydeck)
    =+  second=(snag 1 mydeck)
    ?:  =(val.prev val.first)
      ?:  =(val.first val.second)
        [~ [rank [prev first second ~] ~]]
      ?:  =((lent mydeck) 2)
        ~
      $(prev second, mydeck (slag 2 mydeck))
    $(prev first, mydeck (slag 1 mydeck))
  ++  is-straight
    :: check the first 4 vals less than max, if not then recurse
    |=  mydeck=deck
    ^-  (unit hval)
    =+  rank=4
    ?:  (lth (lent mydeck) 5)  ~
    ?~  mydeck  ~
    =/  max=@   val.i.mydeck
    ?:  ?&
        (deck-has-val (sub max 1) t.mydeck)
        (deck-has-val (sub max 2) t.mydeck)
        (deck-has-val (sub max 3) t.mydeck)
        (deck-has-val (sub max 4) t.mydeck)
        ==
      [~ [rank ~ ~]]
    $(mydeck t.mydeck)
  ::
  :: call count-suits helper function for each suit
  ++  is-flush
    |=  mydeck=deck
    ^-  (unit hval)
    |^
    =+  rank=5
    ?:  ?|
        (gte (count-suits mydeck %hart) 5)
        (gte (count-suits mydeck %spad) 5)
        (gte (count-suits mydeck %club) 5)
        (gte (count-suits mydeck %diam) 5)
      ==
      [~ [rank ~ ~]]
    ~
    ::
    ++  count-suits
      |=  [mydeck=deck mysuit=suit]
      ^-  @ud
      =/  count=@ud  0
      |-
      ?~  mydeck  count
      ?:  =(sut.i.mydeck mysuit)
        $(mydeck t.mydeck, count +(count))
      $(mydeck t.mydeck)
    --
  ::
  :: assumes sorted deck
  ++  is-full-house
    |=  mydeck=deck
    ^-  (unit hval)
    |^
    =+  rank=6
    =|  isset=(unit hval)
    =|  ispair=(unit hval)
    =|  ret=hval
    =.  rank.ret  rank
    =.  isset  (is-set mydeck)
    ?~  isset  ~
    =.  mydeck  (hand-rmv-set mydeck)
    =.  a.ret  a.u.isset
    =.  ispair  (is-pair mydeck)
    ?~  ispair  ~
    ?:  =(rank.u.ispair 1)
      =.  b.ret  a.u.ispair
      [~ ret]
    ~
    ::
    :: assumes the deck is sorted
    ++  hand-rmv-set
      |=  mydeck=deck
      ^-  deck
      =/  prev=darc  (snag 0 mydeck)
      =.  mydeck  (slag 1 mydeck)
      =/  set-idx=@ud  0
      =.  set-idx
      |-
        =+  first=(snag 0 mydeck)
        =+  second=(snag 1 mydeck)
        ?:  =(val.prev val.first)
          ?:  =(val.first val.second)
            set-idx
          $(prev second, mydeck (slag 2 mydeck), set-idx +(set-idx))
        $(prev first, mydeck (slag 1 mydeck), set-idx +(set-idx))
      (oust [set-idx 3] mydeck)
    --
  ::
  ++  is-four-kind
    |=  mydeck=deck   
    ^-  (unit hval)
    =+  rank=7
    =/  prev=darc  (snag 0 mydeck)
    =.  mydeck  (slag 1 mydeck)
    |-
    ?:  (lte (lent mydeck) 2)
      ~
    =+  first=(snag 0 mydeck)
    =+  second=(snag 1 mydeck)
    =+  third=(snag 2 mydeck)
    ?:  =(val.prev val.first)
      ?:  =(val.first val.second)
        ?:  =(val.second val.third)
          [~ [rank [first second third ~] ~]]
        $(prev third, mydeck (slag 3 mydeck))
      $(prev second, mydeck (slag 2 mydeck))
    $(prev first, mydeck (slag 1 mydeck))
  ++  is-straight-flush
    |=  mydeck=deck
    :: check the first 4 vals less than max, if not then recurse
    ^-  (unit hval)
    |^
    =+  rank=8
    ?:  (lth (lent mydeck) 5)  ~
    ?~  mydeck  ~
    =/  mysuit=suit  sut.i.mydeck
    =/  max=@        val.i.mydeck
    ?:  ?&
        (deck-has-darc t.mydeck [mysuit (sub max 1)])
        (deck-has-darc t.mydeck [mysuit (sub max 2)])
        (deck-has-darc t.mydeck [mysuit (sub max 3)])
        (deck-has-darc t.mydeck [mysuit (sub max 4)])
        ==
      [~ [rank ~ ~]]
    $(mydeck t.mydeck)
    ::
      ++  deck-has-darc
        |=  [mydeck=deck mydarc=darc]
        =?  val.mydarc  =(val.mydarc 1)  14  :: treat 1 as 14
        ^-  ?
        =|  head=darc
        |-
        ?~  mydeck  %.n
        ?:  =(i.mydeck mydarc)
          %.y
        $(mydeck t.mydeck)
      --
  ++  is-royal-flush
    |=  mydeck=deck    
    ^-  (unit hval)
    =+  rank=9
    =|  res=(unit hval)
    =/  topdarc=darc  (snag 0 mydeck)
    =.  res  (is-straight-flush mydeck)
    ?~  res  ~
    ?:  ?&
        :: mydeck is a straight flush and the high card is ace
        =(rank.u.res 8)
        =(val.topdarc 14)
        ==
      [~ [rank ~ ~]]
    ~
  :: end hval rank checking arms.
  --
--
--

