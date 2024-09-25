<script setup>
const props = defineProps({
    src: { type: String, required: true },
    label: { type: String, required: true },
    url: { type: String, required: true }
});
</script>

<template>
    <figure>
        <a :href="url">
            <img :src="src" :alt="label" />
        </a>
        <figcaption>
            <span>{{ label }}</span>
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
