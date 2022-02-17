/+  default-agent, hupo, dbug
=,  hupo
|%
:: isdr=isdealround
+$  srvr-pkchlg  [er=@p ee=@p]
+$  state-0  [plrs=(list @p) gsta=pkstat ctnu=(list @p) chlgs=(list srvr-pkchlg) toda=@da isdr=? ingame=? alive=?]
+$  card  card:agent:gall
--
::
^-  agent:gall
=|  state-0
=*  state  -
%-  agent:dbug
=<
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %.n) bowl)
    hc    ~(. +> bowl)
::
++  on-agent  on-agent:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-save   on-save:def
++  on-fail   on-fail:def
++  on-init
  ^-  (quip card _this)
  =.  ingame.state  %.n
  =.  isdr.state  %.n
  =.  plrs.state   ~
  `this
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  =.  ingame.state  %.n
  =.  isdr.state  %.n
  =.  plrs.state   ~
  `this
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?:  ?&  ?!(=(src.bowl our.bowl))
          ?!(alive.state)
      ==
      :: ignore networked pokes
      ::  if not alive
      `this
  ?+    mark  (on-poke:def mark vase)
    %hupo-action
    =^  cards  state
    (handle-action:hc !<(action vase))
    [cards this]
  ==
++  on-arvo
  ^+  on-arvo:*agent:gall
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  :_  this
    :~  (timeout-card-our our.bowl |)
        (reset-card our.bowl)
    ==
--
::
:: begin helper core
::
|_  bowl=bowl:gall
++  handle-action
  |=  =action
  ^-  (quip card _state)
  ?-  -.action
    %reject     `state
    %welcome    `state
    %goodbye    `state
    %gsta       `state
    %ignore     `state
    ::
    %peek       
      ?.  =(src.bowl our.bowl)
        `state
      ~&  >  state 
      `state
    ::
    %reset
      ?.  =(src.bowl our.bowl)
        `state
      ?:  alive.+.action
        :-  goodbye-cards
          init-state
      =/  ista  init-state
      =.  alive.ista  |
      :-  goodbye-cards
        ista
    ::
    %do-timeout
      ::  if wait: brest^bwait
      ::  else: brest
      ?.  =(src.bowl our.bowl)
        `state
      ?.  +.action
        :_  state  ~[brest]
      =/  brest  brest
      =.  toda.state  update-toda
      :_  state
        :-  brest
        :-  bwait  ~
    ::
    :: if src is in plrs.state
    :: remove src from plrs.state
    :: rest timeout
    :: and wish them farewell (poke with %goodbye)
    %leave
      =+  idx=(find [src.bowl]~ plrs.state)
      ?~  idx  reject-action
      :_  state
        :~  (timeout-card-our our.bowl |)
            (reset-card our.bowl)
        ==

    ::
    :: filter bad input
    :: and handle valid player moves
    %play
      ?.  isdr.state  reject-action
      ::
      ?.  ?&  =((lent plrs.state) 2)
              (lien plrs.state |=(p=@p =(p src.bowl)))
             ==
        reject-action
      ::
      ?.  =(src.bowl ~(get-better hupo gsta.state))
        reject-action
      ::
      :: handle valid player moves
      :: 0: fold
      :: 1: check
      :: 2: call
      :: 3: raise
      ?:  =(0 opt.+.action)
        =.  gsta.state  (~(bet-fold hupo gsta.state) src.bowl)
        process-gsta
      ::
      ?:  =(1 opt.+.action)
        ?.  (~(can-plyr-check hupo gsta.state) src.bowl)
          reject-action 
        =.  gsta.state  (~(bet-check hupo gsta.state) src.bowl)
        process-gsta
      ::
      ?:  =(2 opt.+.action)
        =.  gsta.state  (~(bet-call hupo gsta.state) src.bowl)
        process-gsta
      ::
      ?:  =(3 opt.+.action)
        =.  gsta.state  (~(bet-raise hupo gsta.state) src.bowl amt.+.action)
        process-gsta
      reject-action
    ::
    :: ingame - inbetween hands
    :: server awaits a %continue from each player
    %continue
      ?:  isdr.state  reject-action
      ::
      ?.  ?&  (lien plrs.state |=(p=@p =(p src.bowl)))
              (levy ctnu.state |=(p=@p ?!(=(p src.bowl))))
          ==
        reject-action
      ::
      :: append src to ctnu
      =.  ctnu  (weld ctnu ~[src.bowl])
      ?.  =(2 (lent ctnu))
        `state
      ::
      :: continue to next hand
      =.  gsta.state  (~(reset-deal-round hupo gsta.state) eny.bowl)
      =.  isdr.state  &
      =.  ctnu  ~
      :_  state
        (timeout-card &)^(weld welcome-cards gsta-cards)
    ::
    :: server is a middle man for challenges
    :: must recieve a %challenge from A against B
    ::   and a matching %accept
    %challenge
      ?:  ingame.state  reject-action
      ?.  =(our.bowl +>.action)  reject-action
      ::
      ::  reject if its already in chlgs
      ::
      =|  chlg=srvr-pkchlg
      =.  er.chlg  src.bowl
      =.  ee.chlg  +<.action
      ?.  |-  ^-  ?
            ?~  chlgs  &
            ?:  =(chlg i.chlgs)  |
            $(chlgs t.chlgs)
          reject-action
      ::
      :: its a new challenge
      :: forward to challengee
      =.  chlgs
        (weld chlgs [chlg ~])
      =.  +<.action  src.bowl
      :_  state
        :-  (timeout-card &)
        ~[[%pass /poke-wire %agent [ee.chlg %hupo] %poke %hupo-action !>(action)]]
    ::
    %accept
      ?:  ingame.state  reject-action
      ?.  =(our.bowl +>.action)  reject-action
      =|  chlg=srvr-pkchlg
      =.  ee.chlg  src.bowl
      =.  er.chlg  +<.action
      ?.
          |-  ^-  ?
          ?~  chlgs  |
          ?:  =(chlg i.chlgs)  &
          $(chlgs t.chlgs)
        reject-action
      ::
      :: challenge is in memory 
      :: clear challenges and begin game
      ::
      =.  chlgs  ~
      =.  ingame.state  %.y
      =.  plrs  ~[er.chlg ee.chlg]
      =.  gsta.state
        (init-game:hupo plrs.state eny.bowl 2.000 20)
      =.  isdr.state  &
      :_  state
        (timeout-card &)^(weld welcome-cards gsta-cards)
    ==
::
++  init-state
  ^-  state-0
  =/  s  *state-0
  =.  ingame.s  |
  =.  isdr.s  |
  s
::
++  reject-action
  ^-  (quip card _state)
  :_  state
    ~[[%pass /poke-wire %agent [src.bowl %hupo] %poke %hupo-action !>([%reject ~])]]
::
++  process-gsta
  ?:  ~(is-bet-round-over hupo gsta.state)
    =.  gsta.state  (~(end-bet-round hupo gsta.state) eny.bowl)
    ?:  ~(is-deal-round-over hupo gsta.state)
      =.  gsta.state  (~(end-deal-round hupo gsta.state) eny.bowl)
      =.  isdr.state  |
      ?:  ~(is-game-over hupo gsta.state)
        :_  state
        %+  weld
          :-  (timeout-card |)
              full-gsta-cards 
          [(reset-card our.bowl) ~]
      :_  state
      (timeout-card &)^full-gsta-cards
    :_  state
    (timeout-card &)^gsta-cards
  :_  state
  (timeout-card &)^gsta-cards
::
++  todr
  ^-  @dr
  ~m3
++  update-toda
  ^-  @da
  (add now.bowl todr)
::
++  bwait
  ^-  card
    [%pass /wait/(scot %da toda.state) %arvo %b %wait toda.state]
++  brest
  ^-  card
    [%pass /wait/(scot %da toda.state) %arvo %b %rest toda.state]
++  reset-card
  |=  our=@p
  ^-  card
  [%pass /poke-wire %agent [our %hupo-server] %poke %hupo-action !>([%reset &])]
++  timeout-card
  |=  wait=?
  ^-  card
  [%pass /poke-wire %agent [our.bowl %hupo-server] %poke %hupo-action !>([%do-timeout wait])]
++  timeout-card-our
  |=  [our=@p wait=?]
  ^-  card
  [%pass /poke-wire %agent [our %hupo-server] %poke %hupo-action !>([%do-timeout wait])]
++  welcome-cards
  ^-  (list card)
  =/  plra=@p  (snag 0 plrs.state)
  =/  plrb=@p  (snag 1 plrs.state)
  =/  gsta-a=pkstat  (~(get-plyr-stat hupo gsta.state) plra)
  =/  gsta-b=pkstat  (~(get-plyr-stat hupo gsta.state) plrb)
  :~  [%pass /poke-wire %agent [plra %hupo] %poke %hupo-action !>([%welcome ~])]
      [%pass /poke-wire %agent [plrb %hupo] %poke %hupo-action !>([%welcome ~])]
  ==
++  goodbye-cards
  ^-  (list card)
  ?.  =(2 (lent plrs.state))  ~
  =/  plra=@p  (snag 0 plrs.state)
  =/  plrb=@p  (snag 1 plrs.state)
  :~  [%pass /poke-wire %agent [plra %hupo] %poke %hupo-action !>([%goodbye ~])]
      [%pass /poke-wire %agent [plrb %hupo] %poke %hupo-action !>([%goodbye ~])]
  ==
++  gsta-cards
  ^-  (list card)
  =/  plra=@p  (snag 0 plrs.state)
  =/  plrb=@p  (snag 1 plrs.state)
  =/  gsta-a=pkstat  (~(get-plyr-stat hupo gsta.state) plra)
  =/  gsta-b=pkstat  (~(get-plyr-stat hupo gsta.state) plrb)
  :~  [%pass /poke-wire %agent [plra %hupo] %poke %hupo-action !>([%gsta gsta-a])]
      [%pass /poke-wire %agent [plrb %hupo] %poke %hupo-action !>([%gsta gsta-b])]
  ==
++  full-gsta-cards
  ^-  (list card)
  =/  plra=@p  (snag 0 plrs.state)
  =/  plrb=@p  (snag 1 plrs.state)
  =/  full-gsta  gsta.state
  =.  main.delr.full-gsta  ~
  :~  [%pass /poke-wire %agent [plra %hupo] %poke %hupo-action !>([%gsta full-gsta])]
      [%pass /poke-wire %agent [plrb %hupo] %poke %hupo-action !>([%gsta full-gsta])]
  ==
--

