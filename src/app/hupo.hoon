/+  default-agent, hupo, dbug
=,  hupo
|%
+$  clnt-pkchlg  [who=@p srvr=@p]
+$  state-0  [srvr=(unit @p) gsta=pkstat chlg=(unit clnt-pkchlg)]
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
++  on-arvo   on-arvo:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-save   on-save:def
++  on-fail   on-fail:def
++  on-init
  ^-  (quip card _this)
  `this
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  `this
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  (on-poke:def mark vase)
    %hupo-action
    =^  cards  state
    (handle-action:hc !<(action vase))
    [cards this]
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
  %do-timeout  `state
  %ignore  `state
  %peek
    ?.  =(src.bowl our.bowl)
      `state
    ~&  state
    =/  catch  emit-gsta
    `state
  ::
  %reset
    ?.  =(src.bowl our.bowl)
      `state
    `*state-0
  ::
  %continue
    ?~  srvr.state
      `state
    :_  state
      ~[[%pass /poke-wire %agent [u.srvr.state %hupo-server] %poke %hupo-action !>(action)]]
  ::
  :: state update sent from server
  %gsta
    ?~  srvr.state  `state
    ?:  =(src.bowl u.srvr.state)
      =.  gsta.state  ^-(pkstat +.action)
      =/  catch  emit-gsta
      `state
    `state
  ::
  %play
    ?.  =(src.bowl our.bowl)
      `state
    ::  forward exact action to +.action (host)
    ?~  srvr.state
      `state
    :_  state
      ~[[%pass /poke-wire %agent [u.srvr.state %hupo-server] %poke %hupo-action !>(action)]]
  ::
  %leave
    ?.  =(src.bowl our.bowl)
      `state
    ::  forward exact action to +.action (host)
    ?~  srvr.state
      `state
    :_  state
      ~[[%pass /poke-wire %agent [u.srvr.state %hupo-server] %poke %hupo-action !>(action)]]
  ::
  %reject
    ?~  srvr.state  `state
    ?.  =(src.bowl u.srvr.state)
      `state
    =/  catch  emit-begin
    ~&  >>   '  host rejected poke.'
    =.  catch  emit-end
    `state
  ::
  %welcome
    :: check if valid host
    ?~  chlg  `state
    ?:  ?&
          =(srvr.state ~)
          =(src.bowl srvr.u.chlg)
        ==
      :: set to host
      =.  srvr  [~ src.bowl]
      `state
    `state
  ::
  %goodbye
    :: remove src from host
    ?~  srvr.state  `state
    ?:  =(u.srvr.state src.bowl)  ::i.e. this poke is from our host
      :: reset state to bunt
      =/  catch  emit-begin
      ~&  >  'disconnected from:'
      ~&  >>  src.bowl
      =.  catch  emit-end
      `*state-0
    `state
  ::
  :: if from src
  ::  record chlg and forward to host 
  :: if from host
  ::  emit challenge to user  
  %challenge 
    ?:  ?!(=(~ srvr.state))  `state
    ?:  =(src.bowl our.bowl)
      =.  chlg.state  [~ [+<.action +>.action]]
      :_  state
        ~[[%pass /poke-wire %agent [+>.action %hupo-server] %poke %hupo-action !>(action)]]
    ::
    ?:  =(src.bowl +>.action)
      :: challenge is forwarded from host
      =/  catch  emit-begin
      ~&  >    'youve been challenged by'
      ~&  >>   who.+.action
      ~&  >    'to play on'
      ~&  >>   src.bowl
      =/  catch  emit-end
      `state
    `state
  ::
  %accept
    ?:  ?!(=(~ srvr.state))  `state
    ?.  =(src.bowl our.bowl)  `state
    =.  chlg.state  [~ [+<.action +>.action]]
    :_  state
      ~[[%pass /poke-wire %agent [+>.action %hupo-server] %poke %hupo-action !>(action)]]
  ==
++  emit-begin
  ~&  >>>  ' hupo..............................................................'
  ~
++  emit-end
  ~&  >>>  '/hupo..............................................................'
  ~
++  emit-line
  ~&  >>>  '...................................................................'
  ~
++  emit-line-g
  ~&  >  '...................................................................'
  ~
++  emit-open
  ~&  >  '                                                                   '
  ~
++  emit-gsta
  :: p1 who
  :: p1 has  |  p1 pokt
  :: p1 bet  |
  ::         |  bord
  :: p2 bet  |
  :: p2 has  |  p2 pokt
  :: p2 who
  :: -
  :: info
  ::
  =*  gsta  gsta.state
  ?:  =(gsta *pkstat)
    =/  catch  emit-begin
    ~&  >>   ' no current game.'
    =.  catch  emit-end
    ~
  =/  pa  (snag 0 plrs.gsta)
  =/  pb  (snag 1 plrs.gsta)
  :: reorient players if necessary
  =/  switch-plyr  =(who.pa our.bowl)
  =/  aux  pb
  =?  pb  switch-plyr  pa
  =?  pa  switch-plyr  aux
  ::
  :: start printing
  =|  catch=*
  =.  catch  emit-begin
  :: TODO how to display this without "u="
  ~&  >>>  u.+.srvr.state
  =.  catch  emit-line
  ~&  >>  who.pa
  ~&  >>  move.pa
  ~&  >>  :-  has.pa  bet.pa
  ~&  >>  (deck-to-tape:hupo pokt.pa 2)
  =.  catch  emit-line-g
  ~&  >  rwnd.meta.gsta
  =.  catch  emit-open
  =.  catch  emit-open
  ~&  >>  (deck-to-tape:hupo bord.delr.gsta 5)
  =.  catch  emit-open
  =.  catch  emit-open
  =.  catch  emit-open
  =.  catch  emit-line-g
  ~&  >>  (deck-to-tape:hupo pokt.pb 2)
  ~&  >>  :-  has.pb  bet.pb
  ~&  >>  move.pb
  ~&  >>  who.pb
  =.  catch  emit-line
  ?:  =(rwnd.meta.gsta %showdown)
    ~&  >  "winner:"
    ~&  >>  (snag 0 wnrs.meta.gsta)
    ?.  =(1 (lent wnrs.meta.gsta))
      =/  catch  emit-end  ~
    =/  wnr  (snag 0 wnrs.meta.gsta)
    =/  wnrplr  (~(get-plyr hupo gsta) who.wnr)
    ~&  >>  (hval-to-tape:hupo hval.wnrplr)
    =/  catch  emit-end
    ~
  ~&  >  "waiting on bet from:"
  =/  betr  (snag btwho.meta.gsta plrs.gsta)
  ~&  >>  who.betr
  =/  catch  emit-end
  ~
--

