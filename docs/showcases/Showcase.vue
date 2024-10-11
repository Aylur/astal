<script setup>
defineProps({
    src: { type: String, required: true },
    url: { type: String, required: true },
    icon: { type: String, required: false, default: "" },
    title: { type: String, required: false, default: "" },
    description: { type: String, required: false, default: "" },
    author: { type: String, required: true },
})
</script>

<template>
    <figure>
        <div class="image-wrapper">
            <img :src="src" :alt="title">
            <div class="overlay">
                <div class="text-content">
                    <h3 v-if="title.length">
                        {{ title }}
                    </h3>
                    <h3 v-else>
                        {{ author }}'s dotfiles
                    </h3>
                    <p v-if="description">
                        {{ description }}
                    </p>
                    <span class="author">— {{ author }}</span>
                    <a
                        :href="url"
                        target="_blank"
                        class="setup-button"
                    >View Setup</a>
                </div>
            </div>
            <i
                v-if="icon"
                :class="icon"
                class="language-icon"
            />
        </div>
    </figure>
</template>

<style scoped>
figure {
    display: flex;
    flex-direction: column;
    align-items: flex-start;
    padding: 1rem;
    border-radius: 1rem;
    background-color: var(--vp-c-bg-soft);
    transition:
        box-shadow 0.3s ease,
        transform 0.3s ease;
}

figure:hover {
    box-shadow: 0 1.5rem 3rem rgba(0, 0, 0, 0.2);
    transform: translateY(-1rem);
}

.image-wrapper {
    position: relative;
    width: 100%;
    border-radius: 0.5rem;
    overflow: hidden;
}

figure img {
    width: 100%;
    transition: filter 0.3s ease;
}

figure .overlay {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background-color: rgba(0, 0, 0, 0.5);
    display: flex;
    align-items: center;
    justify-content: flex-start;
    padding: 1rem;
    opacity: 0;
    transition:
        opacity 0.3s ease,
        backdrop-filter 0.01s ease;
}

figure:hover .overlay {
    opacity: 1;
    backdrop-filter: blur(1rem);
}

.text-content {
    color: white;
    text-align: left;
    opacity: 0;
    transform: translateX(-5rem);
    transition:
        transform 0.6s cubic-bezier(0.22, 1, 0.36, 1),
        opacity 0.6s ease;
}

figure:hover .text-content {
    transform: translateX(2.5rem);
    opacity: 1;
}

.text-content h3 {
    margin: 0;
    font-size: 2rem;
}

.text-content p {
    margin: 0.5rem 0;
    font-size: 1.1rem;
}

.text-content .author {
    margin-top: 0.5rem;
    text-align: left;
    display: block;
    position: relative;
    font-style: italic;
    font-size: 1rem;
    color: #f0f0f0;
}

figure .language-icon {
    position: absolute;
    bottom: 0.5rem;
    right: 0.5rem;
    padding: 0.5rem;
    background: var(--vp-c-bg-soft);
    border-radius: 100%;
    font-size: 1rem;
    z-index: 1;
}

.setup-button {
    margin-top: 2rem;
    padding: 0.5rem 1rem;
    background-color: var(--vp-c-brand);
    color: white;
    text-decoration: none;
    border-radius: 0.5rem;
    font-weight: bold;
    display: inline-block;
    font-size: 1rem;
    transition: background-color 0.3s ease;
}

.setup-button:hover {
    background-color: darken(var(--vp-c-brand), 10%);
}
</style>
