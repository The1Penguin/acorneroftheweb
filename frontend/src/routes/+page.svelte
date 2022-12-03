
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

<div class="flex flex-col items-center">
  <article class="flex items-center prose md:prose-lg lg:prose-xl">
    <p>
      Hello and welcome to this corner of the web.
    </p>
  </article>

  <div class="grid-rows-3">
  {#await numbers then nums}
    {#each nums as outer}
      <div class="flex">
      {#each outer as inner}
        <div class="grid-rows-3 m-1">
        {#each inner as outer2}
          <div class="flex">
          {#each outer2 as inner2}
              {#if inner2.tag == 'Val'}
                <div class="btn btn-square btn-success">
                {inner2.contents}
                </div>
              {:else}
                <div class="btn btn-square btn-secondary">

                </div>
              {/if}
          {/each}
          </div>
        {/each}
        </div>
      {/each}
      </div>
    {/each}
  {/await}
  </div>
</div>
