import sys
import re

def clean_app_r(filepath, mode):
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    epi_removes = [
        "Ensayos_Clinicos_1", "Ensayos_Clinicos_2", "NNT_EC_PT", 
        "Nomograma_de_Fagan", "Pruebas_Diagnosticas_1", "Pruebas_Diagnosticas_2", 
        "IC_Mediana", "Kappa", "CCI_multiples_obs", "Tamano_Muestral"
    ]
    
    # We remove Ensayos, Diagnosticas, Stat, and Sample Size from EPI
    if mode == "epi":
        # Remove sources
        for app in epi_removes:
            content = re.sub(r'source\("apps/' + app + r'/app\.R"\)\n?', '', content)
        
        # Remove menu_ec block
        content = re.sub(r'\s*nav_menu\(\s*title = span\(id = "menu_ec",.*?\n\s*\),', '', content, flags=re.DOTALL)
        # Remove menu_diag block
        content = re.sub(r'\s*nav_menu\(\s*title = span\(id = "menu_diag",.*?\n\s*\),', '', content, flags=re.DOTALL)
        # Remove menu_stat block
        content = re.sub(r'\s*nav_menu\(\s*title = span\(id = "menu_stat",.*?\n\s*\),', '', content, flags=re.DOTALL)
        # Remove menu_sample block (doesn't have a trailing comma since it's the last one)
        content = re.sub(r'\s*nav_panel\(\s*title = span\(id = "menu_sample".*?\n\s*\)\n', '\n', content, flags=re.DOTALL)
        # Clean up possible trailing commas before the closing paren of page_navbar
        content = re.sub(r',\s*\n\)', '\n)', content)
        
        # Remove server calls
        server_calls = [
            "fagan_Server", "ensayos_clinicos_1_Server", "ensayos_clinicos_2_Server",
            "nnt_ec_pt_Server", "pruebas_diagnosticas_1_Server", "pruebas_diagnosticas_2_Server",
            "ic_mediana_Server", "kappa_Server", "cci_multi_Server", "tamano_muestral_Server"
        ]
        for call in server_calls:
            content = re.sub(r'\s*' + call + r'\(.*?\)\n?', '\n', content)

    elif mode == "stat":
        stat_keeps = epi_removes
        
        # Remove sources not in stat_keeps
        # Find all sources
        sources = re.findall(r'source\("apps/(.*?)/app\.R"\)', content)
        for app in sources:
            if app not in stat_keeps:
                content = re.sub(r'source\("apps/' + re.escape(app) + r'/app\.R"\)\n?', '', content)
                
        # Remove menu_ci block
        content = re.sub(r'\s*nav_menu\(\s*title = span\(id = "menu_ci",.*?\n\s*\),', '', content, flags=re.DOTALL)
        # Remove menu_coh block
        content = re.sub(r'\s*nav_menu\(\s*title = span\(id = "menu_coh",.*?\n\s*\),', '', content, flags=re.DOTALL)
        # Remove menu_cc block
        content = re.sub(r'\s*nav_menu\(\s*title = span\(id = "menu_cc",.*?\n\s*\),', '', content, flags=re.DOTALL)
        # Remove menu_trans block
        content = re.sub(r'\s*nav_menu\(\s*title = span\(id = "menu_trans",.*?\n\s*\),', '', content, flags=re.DOTALL)
        # Remove menu_ecol block
        content = re.sub(r'\s*nav_menu\(\s*title = span\(id = "menu_ecol",.*?\n\s*\),', '', content, flags=re.DOTALL)
        # Remove menu_other block
        content = re.sub(r'\s*nav_menu\(\s*title = span\(id = "menu_other",.*?\n\s*\),', '', content, flags=re.DOTALL)
        
        # Ensure proper trailing comma cleanup
        content = re.sub(r',\s*\n\)', '\n)', content)

        # Remove server calls not in stat
        server_calls_to_remove = [
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
        for call in server_calls_to_remove:
            content = re.sub(r'\s*' + call + r'\(.*?\)\n?', '\n', content)

    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(content)

if __name__ == "__main__":
    clean_app_r("api_epi/app.R", "epi")
    clean_app_r("api_stat/app.R", "stat")
