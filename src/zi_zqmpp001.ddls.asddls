@AbapCatalog.sqlViewName: 'ZIV_ZQMPP001'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Abertura dos processos'

                
@ObjectModel.semanticKey: 'Procid'
                
@ObjectModel.modelCategory: #BUSINESS_OBJECT 
@ObjectModel.compositionRoot: true
@ObjectModel.transactionalProcessingEnabled: true  
@ObjectModel.writeActivePersistence: 'ZQMPP001'
                
@ObjectModel.createEnabled: true
@ObjectModel.deleteEnabled: true 
@ObjectModel.updateEnabled: true

@Search.searchable: true
define view ZI_ZQMPP001 as select from zqmpp001 as AberturaProcesso
association [1] to Z_APOQOP_VH_AUFTYP as _CategoriaOrdem on _CategoriaOrdem.Autyp =  $projection.Autyp
association [1] to Z_APOQOP_VH_AUART as _TipoOrdem on _TipoOrdem.Auart =  $projection.Auart
association [1] to I_BusinessArea as _Divisao on _Divisao.BusinessArea = $projection.Gsber
association [1] to I_MaterialVH as _Material on _Material.Material = $projection.Plnbez
association [1] to I_UnitOfMeasure as _UOM on _UOM.UnitOfMeasure = $projection.Meins
association [*] to I_Order as _Orders 
    on
          _Orders.OrderType = $projection.Auart and
          _Orders.OrderCategory = $projection.Autyp
          // @todo Filtrar por AUFK-ERDAT = data_execucao
{
    @ObjectModel.readOnly: true
    key db_key as db_key,
    
    @Search.defaultSearchElement: true
    @Search.fuzzinessThreshold: 0.8
    @ObjectModel.readOnly: true
    cast(procid as abap.char( 10 )) as Procid,

    @ObjectModel.foreignKey.association: '_CategoriaOrdem'
    @ObjectModel.text.association : '_CategoriaOrdem'
    autyp as Autyp,

    @ObjectModel.foreignKey.association: '_Divisao'
    gsber as Gsber,

    @ObjectModel.foreignKey.association: '_TipoOrdem'
    @ObjectModel.text.association : '_TipoOrdem'
    auart as Auart,

    cckey as Cckey,
    
    @ObjectModel.foreignKey.association: '_Material'
    plnbez as Plnbez,

    ltext as Ltext,
    
    data_execucao as DataExecucao,
    
    @Semantics.quantity.unitOfMeasure: 'Meins'
    menge as Menge,
    
    @Semantics.unitOfMeasure: true
    @ObjectModel.foreignKey.association: '_UOM'
    meins as Meins,
    
    @ObjectModel.readOnly: true
    timestamp_execucao as TimestampExecucao,
    
    @Semantics.user.createdBy: true
    @ObjectModel.readOnly: true
    ernam as Ernam,
    @Semantics.systemDate.createdAt: true
    @ObjectModel.readOnly: true
    erdat as Erdat,
    
    @Semantics.user.lastChangedBy: true
    @ObjectModel.readOnly: true
    aenam as Aenam,
    @Semantics.systemDate.lastChangedAt: true
    @ObjectModel.readOnly: true
    aedat as Aedat,
    
    _CategoriaOrdem,
    _TipoOrdem,
    _Divisao,
    _Material,
    _UOM,
    _Orders
}
