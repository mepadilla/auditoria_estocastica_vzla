#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug  7 12:07:25 2026

@author: meppadilla
"""

# =====================================================================
# SIMULACIÓN NUMÉRICA DE ESTABILIDAD DE TENSIÓN (CURVAS P-V)
# Sistema Interconectado Nacional (SEN) - Venezuela
# =====================================================================

import numpy as np
import matplotlib.pyplot as plt

def generate_pv_curve_venezuela():
    # 1. Parámetros equivalentes de la Red Troncal 765 kV
    V_S = 765.0  # kV (Voltaje constante en el nodo emisor: Guri)
    
    # Impedancia equivalente (R y X) ajustada para representar el corredor 
    # Centro-Occidente (líneas paralelas con longitud equivalente de ~800 km)
    X_eq = 45.0  # Ohms (Reactancia)
    R_eq = 4.0   # Ohms (Resistencia)
    
    # 2. Factores de potencia de la carga
    # Representa la demanda inductiva sin compensación termoeléctrica local
    pf_values = [0.98, 0.95, 0.90]
    colors = ['green', 'blue', 'red']
    labels = ['fp = 0.98 (Ideal)', 'fp = 0.95 (Típico)', 'fp = 0.90 (Crítico)']
    
    # Vector de Potencia Activa (MW) desde 0 hasta 15.000 MW
    P_range = np.linspace(0, 15000, 1000)
    
    # Inicialización del entorno gráfico
    plt.figure(figsize=(12, 7))
    
    # 3. Iteración sobre los distintos escenarios de Factor de Potencia
    for pf, color, label in zip(pf_values, colors, labels):
        theta = np.arccos(pf)
        V_R_vals = []
        
        for P in P_range:
            # Cálculo de la Potencia Reactiva (MVAR) demandada
            Q = P * np.tan(theta)
            
            # Formulación de la ecuación de flujo de potencia para sistema de 2 barras
            # Estructura: a*x^2 + b*x + c = 0, donde x = (V_R)^2
            a = 1.0
            b = 2 * (P * R_eq + Q * X_eq) - V_S**2
            c = (P**2 + Q**2) * (R_eq**2 + X_eq**2)
            
            discriminant = b**2 - 4*a*c
            
            # Evaluación del discriminante (Condición matemática de colapso)
            if discriminant >= 0:
                V_R_sq = (-b + np.sqrt(discriminant)) / (2*a)
                if V_R_sq >= 0:
                    V_R_vals.append(np.sqrt(V_R_sq))
                else:
                    V_R_vals.append(np.nan)
            else:
                # El discriminante negativo indica que la matriz Jacobiana 
                # diverge. Matemáticamente no hay solución de voltaje posible.
                V_R_vals.append(np.nan)
                
        # Graficar la curva P-V para el factor de potencia actual
        plt.plot(P_range, V_R_vals, color=color, linewidth=2.5, label=label)
        
        # 4. Encontrar y marcar el punto exacto de colapso (Nariz de la curva)
        valid = [i for i, v in enumerate(V_R_vals) if not np.isnan(v)]
        if valid:
            max_idx = valid[-1]
            plt.scatter(P_range[max_idx], V_R_vals[max_idx], color=color, s=80, zorder=5)

    # =====================================================================
    # ESTILOS, ANOTACIONES Y CORTES ANALÍTICOS
    # =====================================================================
    
    # Zona de Operación Segura (> 0.9 p.u.)
    plt.axhspan(765*0.9, 765*1.05, color='lightgreen', alpha=0.15, 
                label='Zona de Operación Segura (> 0.9 p.u.)')
    
    # Corte 1: Narrativa Oficial (14.000 MW)
    plt.axvline(x=14000, color='gray', linestyle='--', linewidth=2)
    plt.annotate('Narrativa Oficial\n(14.000 MW)\n[COLAPSO TOTAL]', 
                 xy=(14000, 400), xytext=(12000, 450),
                 arrowprops=dict(facecolor='black', shrink=0.05, width=1.5, headwidth=8),
                 bbox=dict(boxstyle="round,pad=0.3", fc="white", ec="red", lw=1.5),
                 fontsize=11, fontweight='bold', color='red', ha='center')
                 
    # Corte 2: Estimación Modelo Satelital (6.315 MW)
    plt.axvline(x=6315, color='purple', linestyle='-.', linewidth=2)
    plt.annotate('Estimación Modelo Satelital\n(6.315 MW)\n[LÍMITE ESTABLE]', 
                 xy=(6315, 765*0.92), xytext=(4500, 550),
                 arrowprops=dict(facecolor='black', shrink=0.05, width=1.5, headwidth=8),
                 bbox=dict(boxstyle="round,pad=0.3", fc="white", ec="purple", lw=1.5),
                 fontsize=11, fontweight='bold', color='purple', ha='center')

    # Configuración de los Ejes
    plt.title('Curvas P-V (Curvas de Nariz) del Sistema Interconectado Nacional\nDemostración Matemática del Colapso por Déficit de Potencia Reactiva', 
              fontsize=14, fontweight='bold')
    plt.xlabel('Potencia Activa Demandada (MW)', fontsize=12, fontweight='bold')
    plt.ylabel('Voltaje en el Extremo Receptor $V_R$ (kV)', fontsize=12, fontweight='bold')
    plt.grid(True, linestyle=':', alpha=0.7)
    
    # Límites del gráfico
    plt.xlim(0, 15000)
    plt.ylim(300, 800)
    plt.legend(loc='lower left', fontsize=11)
    
    # Renderizado y guardado
    plt.savefig('curvas_pv_colapso.png', dpi=300, bbox_inches='tight')
    plt.show()

# Llamada a la función
if __name__ == '__main__':
    generate_pv_curve_venezuela()