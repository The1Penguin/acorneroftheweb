
<script lang="ts">
  import { onMount } from "svelte"
  let numbers = [];
  let range = [1,2,3,4,5,6,7,8,9];
  onMount(async () => {
      const resp = await fetch('http://localhost:3000/generate');
      numbers = resp.json();
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

  {#await numbers}
  <p>Loading board</p>

  {:then nums}
  <div class="grid grid-cols-9 grid-rows-9">
    {#each nums as outer}
      {#each outer as inner}
        {#if inner.tag == 'Val'}
          <div class="m-0.5 btn btn-square btn-success btn-xs md:btn-md">
          {inner.contents}
          </div>
        {:else}
          <div class="m-0.5 btn btn-square btn-secondary btn-xs md:btn-md">

          </div>
        {/if}
      {/each}
    {/each}
  </div>

  {/await}

  <div class="m-3">
    {#each range as numbs}
      <div class="m-0.5 btn btn-square btn-info btn-xs md:btn-md">
        {numbs}
      </div>
    {/each}
  </div>

  <div>
    <form>
      <button type="submit" class="btn btn-primary">Submit to verify</button>
    </form>
  </div>
</div>
