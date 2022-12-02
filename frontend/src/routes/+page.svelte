
<script lang="ts">
  import { onMount } from "svelte"
  let numbers = [];

  onMount(async () => {
      const resp = await fetch('http://localhost:3000/generate');
      numbers = await resp.json();
    });
</script>

<head>
  <title>
    acorneroftheweb
  </title>
</head>

<article class="flex justify-evenly prose md:prose-lg lg:prose-xl">
  <p>
    Hello and welcome to this corner of the web.
  </p>
</article>

<pre>{JSON.stringify(numbers, null, 2)}</pre>
