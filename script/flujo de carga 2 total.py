#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug  7 12:12:53 2026

@author: meppadilla
"""

import pandapower as pp
import numpy as np

np.Inf = np.inf  # Parche rápido de compatibilidad para NumPy 2.0+

def simulacion_topologica_sen(demanda_nacional_mw):
    print(f"\n{'='*60}")
    print(f" SIMULACIÓN DE FLUJO DE CARGA: {demanda_nacional_mw} MW")
    print(f"{'='*60}")
    
    # 1. Crear una red eléctrica vacía
    net = pp.create_empty_network()

    # =========================================================
    # 2. DEFINICIÓN DE NODOS (SUBESTACIONES CRÍTICAS)
    # =========================================================
    # Eje Troncal 765 kV
    b_guri      = pp.create_bus(net, vn_kv=765, name="01. Guri (Slack)")
    b_malena    = pp.create_bus(net, vn_kv=765, name="02. Malena (Edo. Bolívar)")
    b_sgeronimo = pp.create_bus(net, vn_kv=765, name="03. San Gerónimo B (Edo. Guárico)")
    b_arenosa   = pp.create_bus(net, vn_kv=765, name="04. La Arenosa (Edo. Carabobo)")
    b_yaracuy7  = pp.create_bus(net, vn_kv=765, name="05. Yaracuy 765kV")
    
    # Eje Occidente 400 kV
    b_yaracuy4  = pp.create_bus(net, vn_kv=400, name="06. Yaracuy 400kV (Rebaje)")
    b_tablazo   = pp.create_bus(net, vn_kv=400, name="07. El Tablazo (Edo. Zulia)")

    # =========================================================
    # 3. GENERACIÓN BASE Y TRANSFORMACIÓN
    # =========================================================
    # El Guri dicta el voltaje de referencia (1.0 p.u.) y suple toda la energía
    pp.create_ext_grid(net, bus=b_guri, vm_pu=1.0, name="Complejo Hidroeléctrico Guri")
    
    # Transformador de rebaje 765/400 kV en Yaracuy para enviar a Occidente
    pp.create_transformer_from_parameters(
        net, hv_bus=b_yaracuy7, lv_bus=b_yaracuy4, sn_mva=1500, vn_hv_kv=765, 
        vn_lv_kv=400, vkr_percent=0.2, vk_percent=12.0, pfe_kw=100, i0_percent=0.1, name="Trafo Yaracuy 765/400"
    )

    # =========================================================
    # 4. LÍNEAS DE TRANSMISIÓN (Equivalentes 3 circuitos por tramo)
    # R ~ 0.004 ohm/km, X ~ 0.09 ohm/km (Ya dividido entre 3 circuitos)
    # =========================================================
    R_km, X_km, C_km = 0.004, 0.09, 39.0
    
    pp.create_line_from_parameters(net, b_guri, b_malena, length_km=250, r_ohm_per_km=R_km, x_ohm_per_km=X_km, c_nf_per_km=C_km, max_i_ka=12.0, name="Guri-Malena")
    pp.create_line_from_parameters(net, b_malena, b_sgeronimo, length_km=300, r_ohm_per_km=R_km, x_ohm_per_km=X_km, c_nf_per_km=C_km, max_i_ka=12.0, name="Malena-S.Geronimo")
    pp.create_line_from_parameters(net, b_sgeronimo, b_arenosa, length_km=250, r_ohm_per_km=R_km, x_ohm_per_km=X_km, c_nf_per_km=C_km, max_i_ka=12.0, name="S.Geronimo-Arenosa")
    pp.create_line_from_parameters(net, b_arenosa, b_yaracuy7, length_km=100, r_ohm_per_km=R_km, x_ohm_per_km=X_km, c_nf_per_km=C_km, max_i_ka=12.0, name="Arenosa-Yaracuy")
    
    # Línea 400kV Yaracuy -> Zulia
    pp.create_line_from_parameters(net, b_yaracuy4, b_tablazo, length_km=300, r_ohm_per_km=0.03, x_ohm_per_km=0.28, c_nf_per_km=13.0, max_i_ka=4.0, name="Yaracuy-Tablazo (400kV)")

    # =========================================================
    # 5. DISTRIBUCIÓN DE DEMANDA SIN RESPALDO TÉRMICO
    # Factor de potencia = 0.95 inductivo
    # =========================================================
    tan_phi = np.tan(np.arccos(0.95))
    
    # Repartimos la demanda: 20% Guayana/Oriente (Malena), 40% Centro (Arenosa), 40% Occidente (Tablazo)
    p_oriente = demanda_nacional_mw * 0.20
    p_centro = demanda_nacional_mw * 0.40
    p_occidente = demanda_nacional_mw * 0.40
    
    pp.create_load(net, bus=b_malena, p_mw=p_oriente, q_mvar=p_oriente*tan_phi, name="Carga Oriente")
    pp.create_load(net, bus=b_arenosa, p_mw=p_centro, q_mvar=p_centro*tan_phi, name="Carga Centro")
    pp.create_load(net, bus=b_tablazo, p_mw=p_occidente, q_mvar=p_occidente*tan_phi, name="Carga Occidente")

    # =========================================================
    # 6. EJECUCIÓN DEL ALGORITMO NEWTON-RAPHSON
    # =========================================================
    try:
        # Se ejecuta el flujo de carga ac / Newton-Raphson
        pp.runpp(net, algorithm='nr', max_iteration=50)
        
        print("\n[ÉXITO] El algoritmo convergió.")
        print("\n--- PERFILES DE VOLTAJE POR NODO ---")
        # Mostramos los voltajes de todos los nodos ordenados
        for idx in net.res_bus.index:
            nombre = net.bus.loc[idx, 'name']
            voltaje = net.res_bus.loc[idx, 'vm_pu']
            estado = "CRÍTICO" if voltaje < 0.90 else "ESTABLE"
            print(f"{nombre:<35} : {voltaje:.4f} p.u. [{estado}]")
            
        print("\n--- CARGA EN LÍNEAS DE TRANSMISIÓN ---")
        for idx in net.res_line.index:
            nombre = net.line.loc[idx, 'name']
            carga = net.res_line.loc[idx, 'loading_percent']
            print(f"{nombre:<35} : {carga:.2f} %")
            
    except pp.LoadflowNotConverged:
        print("\n[COLAPSO SISTÉMICO - APAGÓN]")
        print("La Matriz Jacobiana se volvió singular.")
        print("Físicamente imposible transmitir esta energía sin colapso de voltaje.")

# =========================================================
# EJECUCIÓN DE LOS DOS ESCENARIOS CLAVE DE LA TESIS
# =========================================================

# 1. Tu estimación satelital
simulacion_topologica_sen(6315)

# 2. La narrativa oficial
simulacion_topologica_sen(14000)