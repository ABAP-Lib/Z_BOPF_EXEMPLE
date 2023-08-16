@AbapCatalog.sqlViewName: 'ZVH_AUFTYP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'VH: Tipo de Ordem'
@Search.searchable: true
define view Z_APOQOP_VH_AUFTYP as select from Zcds_Domain_Fix_Low
{
    key Low as Autyp,
    @Search.defaultSearchElement: true
    @Semantics.text: true
    Text as AutypText
}
where 
    Domname = 'AUFTYP'
