namespace AstalBluetooth {
internal string kebab_case(string pascal_case) {
    StringBuilder kebab_case = new StringBuilder();

    for (int i = 0; i < pascal_case.length; i++) {
        char c = pascal_case[i];

        if (c >= 'A' && c <= 'Z') {
            if (i != 0) {
                kebab_case.append_c('-');
            }

            kebab_case.append_c((char)(c + 32));
        } else {
            kebab_case.append_c(c);
        }
    }

    return kebab_case.str;
}
}
