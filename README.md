# Simple telegram bot for initiative tracking in dnd5e. 
## initial command list
- /create: creates a lobby
- /lobbyinfo: prints essential lobby information, such as lobby id and joined player nicks
- /join (id): adds user to the lobby with selected id
- /add (Name) (Hits) (Armor class) (Initiative): adds new entity to the turn order
- /rm (Name): removes entity from the turn order
- /removelobby: ends the session
- /all: shows all alive entities in turn order in initiative order with essential information such as current hits and AC
- /cur: shows entity which turn is now
- /nxt: passes initiative to the next entity
- /prv: passes initiative to the previous entity
- /dmg (Name) (damage): decreasing entity's hits by the damage
- /heal (Name) (healed): increasing entity's hits by the healed value
