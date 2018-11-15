- Commands longer than 1024 characters
- Persistent storage
- More interesting things on the player (stats, inventory, etc.)
- File hierachy

Monadic thread interface:
- do I need to be able to emit other events on the local thread? Could just call functions...
- Check thread-local state works
- Integrate with rest of the existing game
- Clean up code and files
- Have a way of the global thread sending events to clients
