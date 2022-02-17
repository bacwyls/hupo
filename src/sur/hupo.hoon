|%
+$  suit  ?(%hart %spad %club %diam)
+$  darc  [sut=suit val=@ud]
+$  deck  (list darc)
+$  hval  [rank=@ud a=deck b=deck]
+$  pkmove      ?(%fold %check %call %raise ~)
+$  pkrwnd      ?(%showdown %river %turn %flop %preflop)
+$  pkwinr      [who=@p amt=@ud]  :: one player can win the full pot, or multiple players can split the pot
                                  :: wnrs=(list pkwinr) is set from get-winners and then used for payout
                                  :: set this as a wing so that the poker hosting agent can access winner/payout info
+$  pkmeta      [sbwho=@ud btwho=@ud btctr=@ud sbamt=@ud rwnd=pkrwnd wnrs=(list pkwinr)]
+$  pkdelr      [main=deck bord=deck]                                 :: main is the original private deck, bord is flop etc
+$  pkplyr      [who=@p pokt=deck move=pkmove has=@ud bet=@ud in=? =hval]             ::
+$  pkstat      [plrs=(list pkplyr) delr=pkdelr meta=pkmeta]
+$  action
  $%  [%peek ~]
      [%leave ~]
      [%continue ~]
      [%reject ~]
      [%welcome ~]
      [%goodbye ~]
      [%ignore ~]
      [%reset alive=?]
      [%do-timeout wait=?]
      [%play [opt=@ud amt=@ud]]
        :: options:
        :: 0 fold
        :: 1 check
        :: 2 call
        :: 3 raise by amt
      [%challenge who=@p srvr=@p]
      [%accept who=@p srvr=@p]
      [%gsta gsta=pkstat]
  ==
--

