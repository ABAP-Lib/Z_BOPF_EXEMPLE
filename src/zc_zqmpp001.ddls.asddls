@AbapCatalog.sqlViewName: 'ZCV_ZQMPP001'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Abertura dos processos'

@ObjectModel.transactionalProcessingDelegated: true
                
@ObjectModel.createEnabled: true
@ObjectModel.deleteEnabled: true
@ObjectModel.updateEnabled: true

@UI.headerInfo: { typeName: 'Abertura processo', typeNamePlural: 'Abertura processos' }
@UI.presentationVariant: [{sortOrder: [{by: 'Procid', direction: #DESC }]}]
                
@OData.publish: true
@Search.searchable: true

define view ZC_ZQMPP001 as select from ZI_ZQMPP001
{
    
    @UI.facet: [ 
        { 
            id:              'Main',
            purpose:         #STANDARD,
            type:            #IDENTIFICATION_REFERENCE,
            label:           'Dados Básicos',
            position:        10 
        },
        { 
            id:              'DadosExec',
            purpose:         #STANDARD,
            type:            #FIELDGROUP_REFERENCE,
            label:           'Dados execução',
            targetQualifier: 'DadosExec',
            position:        20 
        },
        {
            id:              'Ordens',
            purpose:         #STANDARD,
            type:            #LINEITEM_REFERENCE,
            label:           'Ordens',
            position:        30,
            targetElement:   '_Orders'
        }

    ]

    @UI.lineItem.position: 10
    @UI.lineItem: [
        { 
            type: #FOR_ACTION,
            position: 1,
            invocationGrouping: #ISOLATED,
            dataAction: 'BOPF:EXECUTAR',
            label: 'Executar',
            importance: #HIGH 
        }
    ]
    @UI.identification: [
        { 
            type: #FOR_ACTION,
            position: 1,
            invocationGrouping: #ISOLATED,
            dataAction: 'BOPF:EXECUTAR',
            label: 'Executar',
            importance: #HIGH 
        }
    ]

    @UI.identification.position: 10
    @ObjectModel.readOnly: true
    @UI.hidden: true
    key db_key as db_key,

    @UI.lineItem.position: 15
    @UI.identification.position: 15
    @ObjectModel.readOnly: true
    Procid as Procid,
    
    @UI.lineItem.position: 20
    @UI.selectionField.position: 20
    @UI.identification.position: 20
    Autyp,
    
    @UI.lineItem.position: 30
    @UI.selectionField.position: 30
    @UI.identification.position: 30
    Gsber,
    
    @UI.lineItem.position: 40
    @UI.selectionField.position: 40
    @UI.identification.position: 40
    Auart,

    @UI.lineItem.position: 50
    @UI.identification.position: 50
    Cckey,

    @UI.lineItem.position: 60
    @UI.selectionField.position: 60
    @UI.identification.position: 60
    Plnbez,

    @UI.lineItem.position: 70
    @UI.selectionField.position: 70
    @UI.identification.position: 70
    Ltext,

    @UI.lineItem.position: 80
    @UI.selectionField.position: 80
    @UI: { fieldGroup:     [ { qualifier: 'DadosExec', position: 10 } ] }
    DataExecucao,
    
    @UI.lineItem.position: 90
    @UI: { fieldGroup:     [ { qualifier: 'DadosExec', position: 20 } ] }
    Menge,
    
    Meins,
    
    @UI.lineItem.position: 110
    @UI: { fieldGroup:     [ { qualifier: 'DadosExec', position: 40 } ] }
    TimestampExecucao,

    @UI.lineItem.position: 120
    @UI.identification.position: 120
    Ernam,

    @UI.lineItem.position: 130
    @UI.identification.position: 130
    Erdat,

    @UI.lineItem.position: 140
    @UI.identification.position: 140
    Aenam,

    @UI.lineItem.position: 150
    @UI.identification.position: 150
    Aedat,
    
    _CategoriaOrdem,
    _TipoOrdem,
    _Divisao,
    _Material,
    _UOM,
    _Orders
}
