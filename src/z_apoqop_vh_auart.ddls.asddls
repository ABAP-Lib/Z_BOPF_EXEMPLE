@AbapCatalog.sqlViewName: 'ZVH_AUART'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'VH: Tipo de Ordem'
@Search.searchable: true
define view Z_APOQOP_VH_AUART as select from t003o 
    left outer join t003p
    on 
        t003o.client = t003p.client and
        t003o.auart = t003p.auart and
        t003p.spras = $session.system_language 
{
    @Search.defaultSearchElement: true
    key t003o.auart as Auart,
    @Search.defaultSearchElement: true
    @Search.fuzzinessThreshold: 0.8
    @Semantics.text: true
    t003p.txt as Txt
}
