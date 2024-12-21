
#ifndef ASTAL_CLIPBOARD_PRIVATE_H
#define ASTAL_CLIPBOARD_PRIVATE_H

#include "astal-clipboard.h"
#include "wlr-data-control-unstable-v1-client.h"

G_BEGIN_DECLS

AstalClipboardSelection *astal_clipboard_selection_new(struct zwlr_data_control_offer_v1 *offer);

struct zwlr_data_control_offer_v1 *astal_clipboard_selection_get_offer(
    AstalClipboardSelection *self);

G_END_DECLS

#endif  // !ASTAL_CLIPBOARD_PRIVATE_H
