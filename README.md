# hupo
urbit heads up poker
  https://urbit.org/grants/apprenticeships/ua-heads-up-poker/

video demonstration:
  https://youtu.be/nMFBsBn7Rf0


### Playing
```
:hupo|help
```
```
>>> ' hupo..............................................................'
>>  ':hupo|help                 :: display help'
>>  ':hupo|reset                :: reset hupo if something goes wrong'
>>  ':hupo|peek                 :: display state of the running hupo app'
>>  ':hupo|challenge ~bep ~bus  :: challenge player ~bep to play on server ~bus'
>>  ':hupo|accept ~run ~bus     :: accept challenge from player ~run to play on server ~bus'
>>  ':hupo|leave                :: disconnect from a host'
>>  ':hupo|fold                 :: in a game, fold'
>>  ':hupo|check                :: in a game, check'
>>  ':hupo|call                 :: in a game, call'
>>  ':hupo|raise 10             :: in a game, raise by 10'
>>  ':hupo|continue             :: after a hand, continue to the next hand'
>>> '/hupo..............................................................'
```

## Server Install
```
|start %hupo-server
```

### Important pokes:
```:hupo-server &hupo-action [%peek ~]```
  See the state of the hupo-server app

```:hupo-server &hupo-action [%reset &]```
  Reset the hupo-server app. kicks any active players.
  Server will be accept new games

```:hupo-server &hupo-action [%reset |]```
  Reset the hupo-server app. kicks any active players.
  Server will be offline until it recieves a ```[%reset &]```


# Known Issues:
```./lib/hupo.hoon:50:    |-  :: TODO this should be a roll```
Many of the recursive algorithms would be better written using list utilities

```./lib/hupo.hoon:274:  :: TODO, account for player with less chips going all in```
If a player goes all-in with less chips than the other, they have to check until showdown.
This should happen automatically.

```./lib/hupo.hoon:551:      :: TODO could add a kicker to gsta here```
If both players have better than high card, and showdown is determined by a kicker card,
the kicker card is not tracked or displayed in UI.


