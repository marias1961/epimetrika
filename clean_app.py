import sys
import re

def filter_menu(content, menus_to_remove):
    for menu in menus_to_remove:
        # Match `nav_menu( title = span(id = "menu_id", "Title"),`
        # and delete everything until the closing parenthesis `  ),`
        # This assumes formatting is consistent with exactly 2 spaces indentation at the end
        pattern = r'\s*nav_menu\(\s*title = span\(id = "' + menu + r'".*?\n\s*\)(?:,\s*\n\s*\)|,\s*\n|\n\s*\)|\n)'
        
        # We need a more robust way to match balanced blocks.
        # Since this is R code, we can read lines and count parenthesis to delete blocks
        pass

def robust_clean_app_r(filepath, mode):
    with open(filepath, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    epi_removes = [
        "Ensayos_Clinicos_1", "Ensayos_Clinicos_2", "NNT_EC_PT", 
        "Nomograma_de_Fagan", "Pruebas_Diagnosticas_1", "Pruebas_Diagnosticas_2", 
        "IC_Mediana", "Kappa", "CCI_multiples_obs", "Tamano_Muestral"
    ]
    
    server_calls_epi = [
        "fagan_Server", "ensayos_clinicos_1_Server", "ensayos_clinicos_2_Server",
        "nnt_ec_pt_Server", "pruebas_diagnosticas_1_Server", "pruebas_diagnosticas_2_Server",
        "ic_mediana_Server", "kappa_Server", "cci_multi_Server", "tamano_muestral_Server"
    ]

    server_calls_stat = [
        "or_rr_Server", "fap_multivariable_Server", "confusion_interaccion_Server",
        "cohortes_emparejados_Server", "cohort_polytomous_risk_Server",
        "cohortes_politomica_Server", "cc_emparejados_Server",
        "conf_inter_graphics_Server", "casos_controles_estrat_Server",
        "cohorte_estrat_pt_Server", "cross_sectional_Server",
        "cohort_strat_count_Server", "cohortes_1_Server", "cohortes_2_Server",
        "cohortes_3_Server", "casos_controles_1_Server", "casos_controles_2_Server",
        "estudios_transversales_Server", "ecologicos_observacionales_Server",
        "ecologico_Server", "ecologicos_agregados_Server", "ecologicos_multivariable_Server",
        "otros_calculos_nnt_Server"
    ]

    new_lines = []
    skip_mode = False
    paren_level = 0
    target_menus_epi = ["menu_ec", "menu_diag", "menu_stat", "menu_sample"]
    target_menus_stat = ["menu_ci", "menu_coh", "menu_cc", "menu_trans", "menu_ecol", "menu_other"]
    
    menus_to_remove = target_menus_epi if mode == "epi" else target_menus_stat
    servers_to_remove = server_calls_epi if mode == "epi" else server_calls_stat

    menu_pattern = re.compile(r'nav_(menu|panel)\(\s*title = span\(id = "([^"]+)"')
    server_pattern = re.compile(r'^\s*([a-zA-Z0-9_]+_Server)\(')
    source_pattern = re.compile(r'source\("apps/([^/]+)/app\.R"\)')

    for line in lines:
        # Check for Sources
        sm = source_pattern.search(line)
        if sm:
            app_name = sm.group(1)
            if mode == "epi" and app_name in epi_removes:
                continue
            if mode == "stat" and app_name not in epi_removes:
                continue
        
        # Check for Menus / Panels to skip
        if not skip_mode:
            mm = menu_pattern.search(line)
            if mm:
                menu_id = mm.group(2)
                if menu_id in menus_to_remove:
                    skip_mode = "ui"
                    paren_level = line.count('(') - line.count(')')
                    continue
            
            # Check for Servers to skip
            svm = server_pattern.search(line)
            if svm:
                server_id = svm.group(1)
                if server_id in servers_to_remove:
                    skip_mode = "server"
                    paren_level = line.count('(') - line.count(')')
                    # Edge case where server call is one line e.g. server_call()
                    if paren_level <= 0:
                        skip_mode = False
                    continue

        if skip_mode:
            paren_level += line.count('(') - line.count(')')
            if paren_level <= 0:
                skip_mode = False
            continue
            
        new_lines.append(line)

    # Post processing trailing commas in UI definition Before Page_navbar closes
    content = "".join(new_lines)
    content = re.sub(r',\s*\n(\s*\))', r'\n\1', content)
    
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(content)

if __name__ == "__main__":
    robust_clean_app_r("api_epi/app.R", "epi")
    robust_clean_app_r("api_stat/app.R", "stat")
