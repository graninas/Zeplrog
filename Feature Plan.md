# Feature Plan

- [ ] (Feature) Property packs: a way to add a pack of properties
      to the type-level descriptions with only one line of code.

      Example:

      ```haskell
      type Door ... = PropDict
           '[ Pack Openable      -- adds open/close states, state val and scripts
           ]
      ```
- [X] (Mechanics) World type-level description & objects position integration
- [ ] (Subsystem) Graphical representation and tiles
- [ ] (Mechanics) Multi-layer world
- [ ] (Feature)   Multiple items in a world cell (graphically)
- [X] (Feature)   Multiple items in a world position
- [ ] (Mechanics) AI: Goals setting & decision making
- [ ] (Subsystem) ASCII output
- [ ] (Subsystem) Graphic output
- [ ] (Feature)   A better deriving / owning / hierarchy mechanism
