# Wipel conventions

Commands that operate on buffers, windows, or tabs act on the active wip by
default. A prefix argument is the explicit escape hatch for handling global
state outside that wip. Wip-management commands such as `wip` and `wip-kill`
are the exception: they intentionally select or manage the global wip list.
