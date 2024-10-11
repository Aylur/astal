<script setup lang="ts">
import showcases from "./showcases"
import Showcase from "./Showcase.vue"
</script>
<template>
    <div class="Showcases">
        <template v-for="(showcase, index) in showcases" :key="index">
            <div v-if="Array.isArray(showcase)" class="row">
                <div
                    v-for="(elem, elemIndex) in showcase"
                    :key="elemIndex"
                    class="item"
                    :class="`grid-${showcase.length}`"
                >
                    <Showcase
                        :src="elem.src"
                        :url="elem.url"
                        :icon="elem.icon"
                        :title="elem.title"
                        :description="elem.description"
                        :author="elem.author"
                    />
                </div>
            </div>
            <Showcase
                v-else
                :src="showcase.src"
                :url="showcase.url"
                :icon="showcase.icon"
                :title="showcase.title"
                :description="showcase.description"
                :author="showcase.author"
            />
        </template>
    </div>
</template>

<style scoped>
.Showcases {
    margin-top: 24px;
    display: flex;
    flex-direction: column;
    gap: 24px;
}

.row {
    display: flex;
    gap: 24px;
}

/* TODO: responsiveness on mobile */
.item.grid-2 {
    width: calc(100% / 2);
}

.item.grid-3 {
    width: calc(100% / 3);
}

.item.grid-4 {
    width: calc(100% / 4);
}
</style>
