# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

- General agent instructions  @doc/AGENTS.md
- Clojure style guide @doc/clojure-style.md
- Significant points about cosheet's architecture @doc/cosheet.md

## Commands

```bash
# Run all tests
lein test

# Run a single test namespace
lein test cosheet2.server.table-render-test

# Run the development server
lein ring server-headless 3000

# Build a standalone executable jar
lein bin
```

## Architecture

Cosheet is a free-form spreadsheet web app. The server maintains a reactive DOM tree and sends incremental updates to the client over AJAX.

### Source layout

- `src/cosheet2/` - Server-side Clojure: reactive engine and server logic
- `src/cosheet2/server/` - HTTP routes, rendering, session management, actions, DB
- `src_cljc/cosheet2/` - Shared Clojure/ClojureScript code (hiccup utils, client utils)
- `src_cljs/cosheet2/` - ClojureScript client (Reagent/React)
- `test/` - Mirrors source structure with `-test` suffix namespaces

### Naming convention

Most functions that may return a Reporter are named with a `-R` suffix (e.g., `ordered-ids-R`, `render-table-DOM-R`, `id->updating-entity-R`). Functions without the suffix typically return plain values.

### Data model (`src/cosheet2/`)

**Stores** (`store.clj`, `store_impl.clj`, `mutable_store_impl.clj`) hold items identified by `ItemId`. The mutable store itself implements the Reporter protocol, so attending to it gives change notifications.

**Entities** (`entity.clj`, `entity_impl.clj`) are a structural view over store data: primitives, objects, links, or elements. An element is a property or qualifier seen from an owning entity. `id->updating-entity-R` returns a reporter that re-fires whenever the entity changes in the store.

**Queries** (`query.clj`, `query_calculator.clj`) find items or elements matching a pattern. `matching-item-ids-R` returns a reporter tracking matching IDs reactively.

### Server rendering pipeline (`src/cosheet2/server/`)

`dom_manager.clj` coordinates a tree of **components**. A component is like a reporter but notifies the client (via AJAX diff) instead of other code. When a component's underlying reporter fires, the dom manager recomputes only that subtree and sends the delta.

Render functions (e.g., `render-item-DOM`, `render-table-DOM-R`) take a spec map and a store and return hiccup DOM or a reporter of hiccup DOM. They use `expr-let` to reactively depend on store data. Component specs pass render functions by reference (`:render-dom render-item-DOM`) so the dom manager calls them lazily and can share subtrees across updates.

`action_data.clj` attaches action descriptors to component specs. When the client interacts with a DOM node, the descriptor tells the server what action to take. `actions.clj` processes those actions (`:set-content`, `:add-element`, `:delete`, etc.) by mutating the store.

`render.clj` is the top-level entry point: `top-level-DOM-R` returns the root reporter for the entire visible DOM.

### Testing

Use `(is (check value pattern))` from `test/cosheet2/test_utils.clj` instead of `(is (= ...))` for structural comparisons. `check` returns a diff on failure. `any` matches anything; `as-set` treats a sequence as a set.
