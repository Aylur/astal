<script setup>
import { defineProps } from 'vue';

const props = defineProps({
    src: { type: String, required: true },
    author: { type: String, required: true },
    url: { type: String, required: true }
});
</script>

<template>
    <figure>
        <a :href="url">
            <img :src="src" :alt="author" />
        </a>
        <figcaption>
            <span>Author: {{ author }}</span>
        </figcaption>
    </figure>
</template>

<style scoped>
img {
    border-radius: 4px;
}

/* same as VPFeature */
figure {
    padding: 0.8rem;
    padding-bottom: 0;
    border-radius: 12px;
    background-color: var(--vp-c-bg-soft);
}

figcaption {
    text-align: center;
    padding-top: .4em;
    padding-bottom: .6em;
}
</style>
