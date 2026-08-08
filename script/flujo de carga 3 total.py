#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug  7 13:03:16 2026

@author: meppadilla
"""

# =====================================================================
# SIMULACIÓN DE RESCATE TÉRMICO ESPORÁDICO EN EL S.E.N.
# Análisis del impacto de la inyección local de MW y MVAR sobre el voltaje
# =====================================================================

import numpy as np
import matplotlib.pyplot as plt

def simular_rescate_termico(demanda_nacional_mw, gen_termica_esporadica_mw):
    """
    Simula el perfil de voltaje en el extremo receptor considerando la 
    reducción de la carga neta a transmitir desde Guri gracias al aporte térmico local.
    """
    # 1. Parámetros de la Red Troncal 765 kV
    V_S = 765.0  # kV (Voltaje constante en Guri)
    X_eq = 45.0  # Ohms (Reactancia equivalente del corredor de transmisión)
    R_eq = 4.0   # Ohms (Resistencia equivalente)
    
    # 2. Potencia Neta a transmitir desde Guayana
    # La potencia local alivia directamente el flujo por las líneas largas
    net_demand_p = max(0.0, demanda_nacional_mw - gen_termica_esporadica_mw)
    
    # Factor de potencia típico de la carga inductiva (0.95)
    pf = 0.95
    theta = np.arccos(pf)
    net_demand_q = net_demand_p * np.tan(theta)
    
    # 3. Resolución de la ecuación cuadrática de flujo de potencia (Voltaje V_R)
    # Forma: a*V_R^4 + b*V_R^2 + c = 0
    a = 1.0
    b = 2 * (net_demand_p * R_eq + net_demand_q * X_eq) - V_S**2
    c = (net_demand_p**2 + net_demand_q**2) * (R_eq**2 + X_eq**2)
    
    discriminant = b**2 - 4*a*c
    
    if discriminant >= 0:
        v_r_sq = (-b + np.sqrt(discriminant)) / (2*a)
        if v_r_sq >= 0:
            return np.sqrt(v_r_sq)
    
    return None  # Si el discriminante es negativo, ocurre colapso (Apagón)

def analizar_escenarios_rescate():
    demanda_total = 6315.0  # MW (Estimación satelital NTL)
    
    # Rango de generación térmica esporádica a evaluar (de 0 a 3000 MW)
    gen_local_range = np.linspace(0, 3000, 100)
    voltajes_resultantes = []
    
    print(f"{'Gen. Local (MW)':<20} | {'Demanda Neta (MW)':<20} | {'Voltaje V_R (kV)':<20} | {'Estado Operativo'}")
    print("-" * 80)
    
    for gen in [0, 500, 1000, 1500, 2000, 2500]:
        net_p = demanda_total - gen
        v = simular_rescate_termico(demanda_total, gen)
        estado = f"{v:.2f} kV (Estable)" if v else "COLAPSO TOTAL (Apagón)"
        v_str = f"{v:.2f}" if v else "0.00"
        print(f"{gen:<20.1f} | {net_p:<20.1f} | {v_str:<20} | {estado}")

    print("-" * 80)

    # 4. Generación de Gráfica de Rescate Térmico
    for gen in gen_local_range:
        voltajes_resultantes.append(simular_rescate_termico(demanda_total, gen))
        
    plt.figure(figsize=(10, 6))
    plt.plot(gen_local_range, voltajes_resultantes, color='blue', linewidth=3, label='Voltaje en Nodo Receptor ($V_R$)')
    
    # Líneas de referencia
    plt.axhline(y=765*0.9, color='red', linestyle='--', label='Límite Crítico de Tensión (0.9 p.u.)')
    plt.axhline(y=765, color='gray', linestyle=':', label='Voltaje Nominal Guri (1.0 p.u.)')
    
    # Marcador del umbral mínimo de rescate necesario
    min_gen_rescate = 0
    for gen, v in zip(gen_local_range, voltajes_resultantes):
        if v is not None and v >= 765 * 0.9:
            min_gen_rescate = gen
            break
            
    plt.scatter([min_gen_rescate], [765*0.9], color='darkred', s=100, zorder=5)
    plt.annotate(f'Umbral Mínimo de Rescate:\n~{min_gen_rescate:.0f} MW térmicos', 
                 xy=(min_gen_rescate, 765*0.9), xytext=(min_gen_rescate + 300, 765*0.75),
                 arrowprops=dict(facecolor='black', shrink=0.05, width=1.5, headwidth=6),
                 bbox=dict(boxstyle="round,pad=0.3", fc="yellow", ec="black", lw=1),
                 fontweight='bold', fontsize=10)

    plt.title('Efecto del Rescate Térmico Esporádico sobre la Tensión del SEN\nDemanda Fija Modelada = 6.315 MW (Modelo NTL)', fontsize=13, fontweight='bold')
    plt.xlabel('Generación Térmica Local Esporádica (MW) [Zulia / Carabobo]', fontsize=11, fontweight='bold')
    plt.ylabel('Tensión en el Extremo Receptor $V_R$ (kV)', fontsize=11, fontweight='bold')
    plt.grid(True, linestyle=':', alpha=0.7)
    plt.ylim(400, 800)
    plt.xlim(0, 3000)
    plt.legend(loc='lower right', fontsize=10)
    
    plt.savefig('rescate_termico_sen.png', dpi=300, bbox_inches='tight')
    plt.show()

if __name__ == '__main__':
    analizar_escenarios_rescate()