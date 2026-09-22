# -*- coding: utf-8 -*-
"""
Generates a synthetic bank statement workbook in the exact layout the Shiny app
expects: one sheet per 4-digit account, a company header in A1, a header row
buried below filler rows, and a handful of the messy artifacts the real export
produces (summary rows with no account number, USD zero-value lines).

All companies, amounts, references and counterparties are invented.

    python fixtures/generate_demo_statement.py
"""
from __future__ import annotations

import random
from datetime import date, timedelta
from pathlib import Path

from openpyxl import Workbook
from openpyxl.styles import Font

SEED = 1701
OUT = Path(__file__).parent / "demo_statement.xlsx"

COMPANIES = {
    "1101": "ALMENA ADMINISTRACION INMOBILIARIA SA DE CV",
    "1102": "ALMENA DESARROLLOS Y PROYECTOS SA DE CV",
    "1103": "INMOBILIARIA LOMA PRIETA SA DE CV",
    "1104": "EDIFICIOS DE LA PRADERA SA DE CV",
    "1105": "BIENES SAN MARCOS SA DE CV",
    "1106": "OPERADORA REGIONAL DE INMUEBLES SA DE CV",
}

COLUMNS = [
    "CUENTA", "FECHA DE OPERACIÓN", "FECHA", "REFERENCIA", "DESCRIPCIÓN",
    "COD. TRANSAC", "SUCURSAL", "DEPÓSITOS", "RETIROS", "SALDO",
    "MOVIMIENTO", "DESCRIPCIÓN DETALLADA",
]

# (short description, detailed description, is_deposit, (min, max))
# Chosen to exercise every branch of the 78 classification rules.
MOVEMENT_TYPES = [
    ("DEPOSITO DE CUENTA DE TERCEROS", "SPEI RECIBIDO DE CUENTA 0123456789", True, (18_000, 240_000)),
    ("DEP.EFECTIVO", "", True, (3_000, 45_000)),
    ("CHEQUE SBC", "ABONO CH/LOCAL", True, (25_000, 180_000)),
    ("CONCENTRACION DE PAGOS", "PAGO DETALLE REF 88213", True, (12_000, 95_000)),
    ("DEPOSITO CHQ.BANCO", "DEPOSITO DE LA CUENTA 0987654321", True, (30_000, 150_000)),
    ("TRASPASO DE CTA", "DE CUENTA CONCENTRADORA", True, (50_000, 400_000)),
    ("PLAZA PASEO ALTAMIRA", "", True, (8_000, 60_000)),
    ("ESTACIONAMIENTO PALT", "", True, (2_000, 22_000)),
    ("DEPOSITO EN EFECTIVO", "SUCURSAL QUERETARO CENTRO", True, (1_500, 18_000)),
    ("COMISION MANEJO DE CUENTA", "COMISION MENSUAL", False, (350, 2_400)),
    ("CARGO POR COMIS", "INCUMPLIMIENTO DE SALDO MINIMO", False, (500, 1_800)),
    ("I.V.A. COMISIONES", "", False, (56, 384)),
    ("Impuesto Sobre la Renta", "PAGO DE IMPUESTOS PROVISIONALES", False, (15_000, 180_000)),
    ("PAGO DE SUA-IMSS", "PAGO DE IMPUESTOS OBRERO PATRONAL", False, (22_000, 95_000)),
    ("TRASPASO A CUENTA PROPIA", "", False, (40_000, 350_000)),
    ("DEPOSITO DE CUENTA PROPIA", "", True, (40_000, 350_000)),
    ("TRASPASO A CUENTA DE TERCEROS", "PAGO A PROVEEDOR REF 4471", False, (9_000, 210_000)),
    ("COMPRA ORDEN DE PAGO SPEI", "PAGO PROVEEDOR MANTENIMIENTO", False, (5_000, 88_000)),
    ("RETIRO DEP. ELECTRONICO", "DE LA EMISORA : NOMINA QUINCENAL", False, (120_000, 480_000)),
    ("INVERSION A PLAZO", "", False, (500_000, 2_500_000)),
    ("DESINV", "", True, (500_000, 2_500_000)),
    ("LIQ.INT.BRUTOS LIQ", "", True, (1_200, 24_000)),
    ("DEV.SPEICUENTA BLOQUEADA", "CUENTA BLOQUEADA POR EL BENEFICIARIO", True, (4_000, 40_000)),
    ("PAGO SERVICIOS", "AGUA Y DRENAJE MUNICIPAL", False, (1_800, 26_000)),
    ("PAGO SERVICIOS", "COMISION FEDERAL DE ELECTRICIDAD", False, (12_000, 140_000)),
    ("PAGO SERVICIOS", "TELECOM SIERRA LINEAS CORPORATIVAS", False, (2_400, 14_000)),
    ("PAGO SERVICIOS", "GAS URBANO SUMINISTRO", False, (3_100, 19_000)),
    ("PAGO SERVICIOS", "ASEGURADORA ISTMO POLIZA DANOS", False, (18_000, 120_000)),
    ("PAGO DE CAPITAL", "", False, (80_000, 600_000)),
    ("PAGO DE INTERESES", "", False, (12_000, 140_000)),
    ("PAGO ARRENDADORA", "", False, (25_000, 90_000)),
    ("CHEQUE", "", False, (6_000, 75_000)),
    ("CERTIFICA.CHQ.", "", False, (400, 1_200)),
]


def build_sheet(wb: Workbook, account: str, company: str, rng: random.Random) -> None:
    ws = wb.create_sheet(title=account)

    # Row 1 — the app parses the company name out of this exact shape:
    # "<4 digits> <COMPANY NAME>, <anything>"
    ws["A1"] = f"{account} {company}, CUENTA DE CHEQUES MONEDA NACIONAL"
    ws["A1"].font = Font(bold=True)
    ws["A2"] = "ESTADO DE CUENTA — PERIODO 01/03/2026 AL 31/03/2026"
    ws["A4"] = "CLIENTE: 000" + account
    ws["A6"] = "SALDO INICIAL"
    ws["B6"] = 1_000_000.00

    # Header row lands on row 14, as in the real export.
    for col, name in enumerate(COLUMNS, start=1):
        cell = ws.cell(row=14, column=col, value=name)
        cell.font = Font(bold=True)

    full_account = f"0280{account}"
    saldo = 1_000_000.00
    row = 15
    start = date(2026, 3, 2)
    movement_no = 1

    for day_offset in range(30):
        day = start + timedelta(days=day_offset)
        if day.weekday() >= 5:            # bank does not post on weekends
            continue
        for _ in range(rng.randint(2, 6)):
            short, detail, is_deposit, (lo, hi) = rng.choice(MOVEMENT_TYPES)
            amount = round(rng.uniform(lo, hi), 2)
            saldo = round(saldo + amount if is_deposit else saldo - amount, 2)

            ws.cell(row=row, column=1, value=full_account)
            ws.cell(row=row, column=2, value=day)
            ws.cell(row=row, column=3, value=day)
            ws.cell(row=row, column=4, value=rng.randint(100_000, 999_999))
            ws.cell(row=row, column=5, value=short)
            ws.cell(row=row, column=6, value=f"T{rng.randint(10, 99)}")
            ws.cell(row=row, column=7, value=rng.choice([1, 14, 27, 33]))
            ws.cell(row=row, column=8, value=amount if is_deposit else None)
            ws.cell(row=row, column=9, value=None if is_deposit else amount)
            ws.cell(row=row, column=10, value=saldo)
            ws.cell(row=row, column=11, value=movement_no)
            ws.cell(row=row, column=12, value=detail)
            row += 1
            movement_no += 1

        # One USD zero line per week — the app drops these.
        if day.weekday() == 2 and rng.random() < 0.4:
            ws.cell(row=row, column=1, value=full_account)
            ws.cell(row=row, column=2, value=day)
            ws.cell(row=row, column=5, value="OPERACION EN DIVISA")
            ws.cell(row=row, column=12, value="$0.00 USD SIN MOVIMIENTO")
            row += 1

    # Summary rows with an empty account number — the app drops these too.
    for label, value in (("DEPÓSITOS", 0), ("OPERACIONES", 0), ("TOTAL", saldo)):
        ws.cell(row=row, column=5, value=label)
        ws.cell(row=row, column=10, value=value)
        row += 1

    for col, width in zip("ABCDEFGHIJKL", (14, 13, 13, 12, 34, 10, 10, 14, 14, 16, 12, 42)):
        ws.column_dimensions[col].width = width


def main() -> None:
    rng = random.Random(SEED)
    wb = Workbook()
    wb.remove(wb.active)

    # A sheet the app must ignore: only 4-digit names are treated as accounts.
    notes = wb.create_sheet(title="PORTADA")
    notes["A1"] = "Synthetic statement — generated data, no real records."

    # The official opening-balance sheet. Without it the pipeline falls back to
    # an estimate and raises a warning — worth seeing both paths.
    saldos = wb.create_sheet(title="Saldo Inicial")
    for col, name in enumerate(("CUENTA", "EMPRESA", "SALDO"), start=1):
        saldos.cell(row=1, column=col, value=name).font = Font(bold=True)
    for i, (account, company) in enumerate(COMPANIES.items(), start=2):
        saldos.cell(row=i, column=1, value=f"0280{account}")
        saldos.cell(row=i, column=2, value=company)
        saldos.cell(row=i, column=3, value=1_000_000.00)
    for col, width in zip("ABC", (16, 46, 16)):
        saldos.column_dimensions[col].width = width

    for account, company in COMPANIES.items():
        build_sheet(wb, account, company, rng)

    wb.save(OUT)
    print(f"Wrote {OUT} ({OUT.stat().st_size / 1024:.0f} KB) "
          f"— {len(COMPANIES)} accounts")


if __name__ == "__main__":
    main()
