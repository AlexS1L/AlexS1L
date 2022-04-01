######################################################################
############### An¨¢lse de Consultoria Estat¨ªsitica ###################
######################################################################
.libPaths("/home/alex/R") #quando n??o conseguir a baixar pacotes no R
rm(list = ls())  ## Limpando o Global Environment
## Biblioteca que ser¨¢ usado no relat¨®rio 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
## Quantidade de  conjunto de dados
vidaacademia <- "/home/alex/Downloads/vida_academica.csv"             # Dados 1
socio_pedido <- "/home/alex/Downloads/socio_pedido1.csv"              # Dados 2
comvestdados <-"/home/alex/Downloads/comvest_pedido1.csv"             # Dados 3
dados_cadastrais <-"/home/alex/Downloads/dados_cadastrais.csv"        # Dados 4
historico_escolar <-"/home/alex/Downloads/historico_escolar.csv"      # Dados 5
resumo_por_perido <- "/home/alex/Downloads/resumo_por_periodo.csv"    # Dados 6
rais_pedido1<-"/home/alex/Downloads/rais_pedido1.csv"                 # Dados 7
###############################################################################
## Primeiro ser¨¢ trabalhado com o conjunto de dados da COMVEST

# Uma an¨¢lise explorat¨®ria no conjunto de dados da COMVEST 
# Leitura
cols<-cols_only(id = 'n',ano_vest = 'n',tipo_ingresso_comvest = 'n',ano_nasc_c = 'n',opc1 = 'n',opc2 = 'n',opc3 = 'n',
                nacionalidade = 'n',pais_nasc = 'c',mun_nasc_c = 'c',uf_nasc_c = 'c',cep_resid_c = 'c',mun_resid_c = 'c',
                uf_resid = 'c',esc_em = 'c',mun_esc_em = 'c',uf_esc_em = 'c',nat_esc_em = 'n',ano_conclu_em_c = 'n',
                cid_inscricao = 'c',instituicao = 'n',sexo = 'n',est_civil = 'n',local_resid = 'n',isento = 'n',
                paais = 'n',raca = 'n',tipo_esc_ef = 'n',tipo_esc_ef_1 = 'n',tipo_esc_ef_2 = 'n',tipo_esc_em = 'n',
                tipo_curso_em = 'n',periodo_em = 'n',cursinho = 'n',cursinho_motivo = 'n',cursinho_tempo = 'n',
                cursinho_tipo = 'n',cursinho_nao_motivo = 'n',univ_outra = 'n',unicamp_motivo = 'n',opc1_motivo_a = 'n',
                opc1_motivo_b = 'n',renda_sm = 'n',renda_sm_a = 'n',renda_sm_b = 'n',renda_sm_c = 'n',renda_sm_d = 'n',
                renda_qtas = 'n',renda_contrib_qtas = 'n',moradia_situacao = 'n',ocup_pai = 'n',ocup_mae = 'n',
                trabalha_pai = 'n',trabalha_mae = 'n',educ_pai = 'n',educ_mae = 'n',trabalha = 'n',contribui_renda_fam = 'n',
                jornal_le = 'n',livros_qtos = 'n',lugar_calmo_casa = 'n',jornal_assina = 'n',revistas_assina = 'n',
                enciclopedia = 'n',atlas = 'n',dicionario = 'n',calculadora = 'n',empr_domest_qtas = 'n',idiomas = 'n',
                internet = 'n',internet_onde = 'n',cozinha_qtas = 'n',sala_qtas = 'n',quarto_qts = 'n',banheiro_qts = 'n',
                radio_qts = 'n',tv_qts = 'n',dvd_vhs_qts = 'n',computador_qtos = 'n',carro_qtos = 'n',geladeira = 'n',
                maq_roupa = 'n',aspirador = 'n',freezer = 'n',maq_louca = 'n',aprov_f1 = 'n',curso_aprovado = 'n',
                curso_matric = 'n',questoes = 'n',not_qui_f1 = 'n',not_geo_f1 = 'n',not_fis_f1 = 'n',not_bio_f1 = 'n',
                not_mat_f1 = 'n',not_his_f1 = 'n',not_red_f1 = 'n',not_apt_mus = 'n',notpad_apt_mus = 'n',nf_f1_f1 = 'n',
                notpad_f1_f1 = 'n',presente_f1 = 'c',nf_f1_f2 = 'n',notpad_f1_f2 = 'n',not_f1_opc2 = 'n',
                notpad_f1_opc2 = 'n',aprov_apt = 'c',not_apt = 'n',notpad_aptidao = 'n',pqui = 'c',not_qui_f2 = 'n',
                notpad_qui = 'n',pgeo = 'c',not_geo_f2 = 'n',notpad_geo = 'n',pfis = 'c',not_fis_f2 = 'n',notpad_fis = 'n',
                pbio = 'c',not_bio_f2 = 'n',notpad_bio = 'n',pmat = 'c',not_mat_f2 = 'n',notpad_mat = 'n',phis = 'c',
                not_his_f2 = 'n',notpad_his = 'n',ppor = 'c',not_por = 'n',notpad_por = 'n',pest = 'c',not_est = 'n',
                notpad_est = 'n',not_cha = 'n',notpad_cha = 'n',not_cn = 'n',notpad_cn = 'n',not_inter = 'n',
                notpad_inter = 'n',not_red_f2 = 'n',notpad_red = 'n',npo1 = 'n',npo2 = 'n',npo3 = 'n',np_unica = 'n',
                area = 'c',grupo1 = 'n',grupo2 = 'n',grupo3 = 'n',clas_opc1 = 'n',clas_opc2 = 'n',clas_opc3 = 'n',
                clacar = 'n',nf_f2_opc1 = 'n',nf_f2_opc2 = 'n',pres_f2_d4 = 'c',nota_enem = 'n',notpad_lc_ve = 'n',
                notpad_mat_ve = 'n',notpad_cn_ve = 'n',notpad_ch_ve = 'n',notpad_red_ve = 'n',not_red_ve = 'n',
                notpad_he_ve = 'n',not_he_ve = 'n',npo1_ve = 'n',npo2_ve = 'n',grupo1_ve = 'n',grupo2_ve = 'n',
                clas_opc1_ve = 'n',clas_opc2_ve = 'n',questoes_vi = 'n',pontuacao_vi = 'n',not_red_vi = 'n',
                not_musica_vi = 'n',nf_opc1_vi = 'n',nf_opc2_vi = 'n',grupo1_vi = 'n',grupo2_vi = 'n',clas_opc1_vi = 'n',
                clas_opc2_vi = 'n',presente_vi = 'n',olimpiada1_vo = 'n',participacao1_vo = 'n',olimpiada2_vo = 'n',
                participacao2_vo = 'n',npo1_vo = 'n',npo2_vo = 'n',grupo1_vo = 'n',grupo2_vo = 'n',clas_opc1_vo = 'n',
                `clas_opc2_vo;` = 'n')

comvest<-read_csv(comvestdados,col_types =cols,na = c("","NA","-",";"),comment = '"')
comvest %>% glimpse()
## An¨¢lise descritiva
# Informa????o do conjunto de dados 
summary(comvestdd$sexo) 
# Histograma dos anos do vestibular
hist(comvestdd$ano_vest, xlab="Ano do vestibular",ylab="Frequ??ncia")





