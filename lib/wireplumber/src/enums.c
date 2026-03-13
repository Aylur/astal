
#include "enums.h"

#include "astal-wp-enum-types.h"
#include "glib-object.h"

GEnumValue *astal_wp_enum_from_string(const gchar *string, GType enum_type) {
    GString *str = g_string_new_take(g_strdup(string));
    g_string_replace(str, "/", "-", 0);

    GEnumClass *enum_class = g_type_class_ref(enum_type);
    GEnumValue *enum_value = g_enum_get_value_by_nick(enum_class, str->str);

    g_string_free(str, TRUE);
    g_type_class_unref(enum_class);

    return enum_value;
}

gchar *astal_wp_enum_to_string(GType enum_type, gint value) {
    GEnumClass *enum_class = g_type_class_ref(enum_type);
    const gchar *nick = g_enum_get_value(enum_class, value)->value_nick;
    g_type_class_unref(enum_class);

    GString *str = g_string_new_take(g_strdup(nick));
    g_string_replace(str, "-", "/", 0);
    return g_string_free(str, FALSE);
}

AstalWpDeviceType astal_wp_device_type_from_string(const gchar *string) {
    GEnumValue *val = astal_wp_enum_from_string(string, ASTAL_WP_TYPE_DEVICE_TYPE);
    return val ? val->value : ASTAL_WP_DEVICE_TYPE_UNKNOWN;
}

AstalWpMediaClass astal_wp_media_class_from_string(const gchar *string) {
    GEnumValue *val = astal_wp_enum_from_string(string, ASTAL_WP_TYPE_MEDIA_CLASS);
    return val ? val->value : ASTAL_WP_MEDIA_CLASS_UNKNOWN;
}

AstalWpMediaRole astal_wp_media_role_from_string(const gchar *string) {
    GEnumValue *val = astal_wp_enum_from_string(string, ASTAL_WP_TYPE_MEDIA_ROLE);
    return val ? val->value : ASTAL_WP_MEDIA_ROLE_UNKNOWN;
}

AstalWpMediaCategory astal_wp_media_category_from_string(const gchar *string) {
    GEnumValue *val = astal_wp_enum_from_string(string, ASTAL_WP_TYPE_MEDIA_CATEGORY);
    return val ? val->value : ASTAL_WP_MEDIA_CATEGORY_UNKNOWN;
}

gchar *astal_wp_media_class_to_string(AstalWpMediaClass media_class) {
    return astal_wp_enum_to_string(ASTAL_WP_TYPE_MEDIA_CLASS, media_class);
}

gchar *astal_wp_media_category_to_string(AstalWpMediaCategory category) {
    return astal_wp_enum_to_string(ASTAL_WP_TYPE_MEDIA_CATEGORY, category);
}

gchar *astal_wp_media_role_to_string(AstalWpMediaRole role) {
    return astal_wp_enum_to_string(ASTAL_WP_TYPE_MEDIA_ROLE, role);
}

gchar *astal_wp_device_type_to_string(AstalWpDeviceType device_type) {
    return astal_wp_enum_to_string(ASTAL_WP_TYPE_DEVICE_TYPE, device_type);
}
