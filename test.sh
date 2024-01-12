echo "erlang:\n" && gleam test --target=erlang \
&& echo "\nnode:\n" && gleam test --target=javascript --runtime=node \
&& echo "\ndeno:\n" && gleam test --target=javascript --runtime=deno