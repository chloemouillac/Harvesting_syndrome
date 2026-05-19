# Packages to import
import pandas as pd
import requests
from bs4 import BeautifulSoup
import re


def fetch_section_text(soup, section_id):
    """Fetches text content from a specified section in the PFAF page by section ID."""
    section = soup.find("span", id = section_id)
    return section.text if section else ''


def parse_uses(uses_text, part_marker, use_marker):
    """Extracts plant part and use lists from the text, separating description if available."""
    
    if not uses_text :
        return '', '', ''
      
    plant_parts, uses_list, description = '', '', uses_text.strip()

    if part_marker in uses_text:
        plant_parts = ", ".join(uses_text.split(part_marker)[1].split(use_marker)[0].split("\xa0\xa0")).strip()
        # extracts a list of plant parts by splitting the input uses_text at part_marker, further splitting it at use_marker, then dividing the resulting part by the separator \xa0\xa0, joining the parts with commas, and removing extra whitespace
    
    if use_marker in uses_text :
        if use_marker !=  "" :
            uses_list_raw = uses_text.split(use_marker)[1]
        else :
            uses_list_raw = uses_text
            
        if bool(re.search(r'[a-z][A-Z]', uses_list_raw)) :
            split = re.split(r'(?<= [a-z])(? = [A-Z])', uses_list_raw)
            uses_list = split[0]
            description = split[1].strip()
            uses_list = ", ".join(uses_list.split("\xa0\xa0")).strip()
    
    
    # In cases when we have only the description and the categories "Parts" and "Uses" are still present but empty
    if use_marker !=  "" :
        if use_marker in uses_text and not bool(re.search(r'[a-z][A-Z]', uses_list_raw)):
            description = uses_text.split(use_marker)[1].strip()
        elif part_marker in uses_text:
            description = uses_text.split(part_marker)[1].strip()
    
    return plant_parts, uses_list, description




def get_plant_info(genus, species):
    """Fetches plant information for a specific genus and species from the PFAF database."""
    pfaf_url = f"https://pfaf.org/user/Plant.aspx?LatinName = {genus}+{species}"
    headers = {"user-agent": "Mozilla/5.0"}
    response = requests.get(pfaf_url, headers = headers)

    if response.status_code !=  200:
        return (genus, species, *['Species not in PFAF database']*8)
    
    soup = BeautifulSoup(response.content, 'html.parser')

    # Edible Uses Section
    edible_text = fetch_section_text(soup, "ContentPlaceHolder1_txtEdibleUses")
    edible_parts, edible_uses, edible_desc = parse_uses(edible_text, "Edible Parts:", "Edible Uses:")

    # Medicinal Uses Section
    medicinal_text = fetch_section_text(soup, "ContentPlaceHolder1_txtMediUses")
    medicinal_parts, medicinal_uses, med_desc = parse_uses(medicinal_text, "NO PART", "medicinally.") # NO PART, because there aren't any medicinal 'parts'

    # Other Uses Section
    other_text = fetch_section_text(soup, "ContentPlaceHolder1_txtOtherUses")
    other_parts, other_uses, other_desc = parse_uses(other_text, "NO PART", "") # NO PART, because there aren't any medicinal 'parts', and "" because there is no separator

    # Special Uses Section
    special_text = fetch_section_text(soup, "ContentPlaceHolder1_txtSpecialUses")
    special_parts, special_uses, special_desc = parse_uses(special_text, "NO PART", "")

    return (
        genus, species,
        edible_parts, edible_uses, edible_desc,
        medicinal_uses, med_desc,
        other_uses, other_desc,
        special_uses, special_desc
    )
